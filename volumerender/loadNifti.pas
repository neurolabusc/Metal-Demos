unit loadNifti;
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface
{$IFDEF FPC}
 {$DEFINE GZIP}
{$ENDIF}
uses
  {$IFDEF GZIP}{$IFDEF FPC}zstream, {$ELSE} zlib,{$ENDIF}{$ENDIF} //Freepascal includes the handy zstream function for decompressing GZip files
  VectorMath, SimdUtils, sysutils,Classes, nifti_types, Math;
//Written by Chris Rorden, released under BSD license
//This is the header NIfTI format http://nifti.nimh.nih.gov/nifti-1/
//NIfTI is popular in neuroimaging - should be compatible for Analyze format
//   http://eeg.sourceforge.net/ANALYZE75.pdf
//NIfTI format images have two components:
// 1.) Header data provides image dimensions and details
// 2.) Image data
//These two components can be separate files: MRI.hdr, MRI.img
//  or a single file with the header at the start MRI.nii
//Note raw image daya begins vox_offset bytes into the image data file
//  For example, in a typical NII file, the header is the first 348 bytes,
//  but the image data begins at byte 352 (as this is evenly divisible by 8)
Type
  TNIfTI = Class(TObject)  // This is an actual class definition :
      // Internal class field definitions - only accessible in this unit
      private
        fMin, fMax, fAutoBalMin, fAutoBalMax, fWindowMin, fWindowMax: single;
        fScale : TVec3;
        fDim: TVec3i;
        fhdr  : TNIFTIhdr;
        fFileName: string;
        fRawVolBytes: TUInt8s;
        fVolRGBA: TRGBAs;
        procedure SetDisplayMinMaxFloat32();
        procedure InitFloat32();
        procedure SetDisplayMinMaxInt16();
        procedure InitInt16();
        procedure SetDisplayMinMaxUint8();
        procedure InitUInt8();
        procedure SetDisplayMinMax(); overload;
        procedure convert2Float();
        function OpenNIfTI(): boolean;
        procedure MakeBorg(voxelsPerDimension: integer);
      public
        property VolumeMin: single read fMin; //darkest voxel in volume
        property VolumeMax: single read fMax; //brightest voxel in volume
        property DisplayMin: single read fWindowMin;
        property DisplayMax: single read fWindowMax;
        property Header: TNIFTIhdr read fhdr;
        property Scale: TVec3 read fScale;
        property Dim: TVec3i read fDim;
        property VolRGBA: TRGBAs read fVolRGBA;
        property Filename: String read fFileName;
        procedure GPULoadDone();
        procedure Load(niftiFileName: string);
        procedure SetDisplayMinMax(newMin, newMax: single); overload;
        function GenerateGradientVolume: TRGBAs;
        constructor Create(); overload;
        constructor Create(niftiFileName: string); overload;
        destructor Destroy; override;
  end;

implementation


const
 kRGBAclear : TRGBA = (r: 0; g: 0; b: 0; a:0);

procedure ShowDebug (lS: AnsiString);
begin
{$IFNDEF WINDOWS} writeln('*****DEBUG : '+lS); {$ENDIF}
end;

Function XYZI (X1,X2,Y1,Y2,Z1,Z2: single; Center: byte): TRGBA;
//gradients in range -1..1
//input voxel intensity to the left,right,anterior,posterior,inferior,superior and center
// Output RGBA image where values correspond to X,Y,Z gradients and ImageIntensity
//AlphaT will make a voxel invisible if center intensity is less than specified value
// Voxels where there is no gradient (no edge boundary) are made transparent
var
  X,Y,Z,Dx: single;
begin
  Result := kRGBAclear;
  if Center < 1 then
    exit; //intensity less than threshold: make invisible
  X := X1-X2;
  Y := Y1-Y2;
  Z := Z1-Z2;
  Dx := sqrt(X*X+Y*Y+Z*Z);
  if Dx = 0 then
    exit;  //no gradient - set intensity to zero.
  result.r :=round((X/(Dx*2)+0.5)*255); //X
  result.g :=round((Y/(Dx*2)+0.5)*255); //Y
  result.b := round((Z/(Dx*2)+0.5)*255); //Z
  result.a := Center;
end;

function Sobel (rawData: TUInt8s; Xsz,Ysz, I : integer; var GradMag: single): TRGBA;
//this computes intensity gradients using 3D Sobel filter.
//Much slower than central difference but more accurate
//http://www.aravind.ca/cs788h_Final_Project/gradient_estimators.htm
var
  Y,Z,J: integer;
  Xp,Xm,Yp,Ym,Zp,Zm: single;
begin
  GradMag := 0;//gradient magnitude
  Result := kRGBAclear;
  if rawData[i] < 1 then
    exit; //intensity less than threshold: make invisible
  Y := XSz; //each row is X voxels
  Z := YSz*XSz; //each plane is X*Y voxels
  //X:: cols: +Z +0 -Z, rows -Y +0 +Y
  J := I+1;
  Xp := rawData[J-Y+Z]+3*rawData[J-Y]+rawData[J-Y-Z]
        +3*rawData[J+Z]+6*rawData[J]+3*rawData[J-Z]
        +rawData[J+Y+Z]+3*rawData[J+Y]+rawData[J+Y-Z];
  J := I-1;
  Xm := rawData[J-Y+Z]+3*rawData[J-Y]+rawData[J-Y-Z]
        +3*rawData[J+Z]+6*rawData[J]+3*rawData[J-Z]
        +rawData[J+Y+Z]+3*rawData[J+Y]+rawData[J+Y-Z];
  //Y:: cols: +Z +0 -Z, rows -X +0 +X
  J := I+Y;
  Yp := rawData[J-1+Z]+3*rawData[J-1]+rawData[J-1-Z]
        +3*rawData[J+Z]+6*rawData[J]+3*rawData[J-Z]
        +rawData[J+1+Z]+3*rawData[J+1]+rawData[J+1-Z];
  J := I-Y;
  Ym := rawData[J-1+Z]+3*rawData[J-1]+rawData[J-1-Z]
        +3*rawData[J+Z]+6*rawData[J]+3*rawData[J-Z]
        +rawData[J+1+Z]+3*rawData[J+1]+rawData[J+1-Z];
  //Z:: cols: +Z +0 -Z, rows -X +0 +X
  J := I+Z;
  Zp := rawData[J-Y+1]+3*rawData[J-Y]+rawData[J-Y-1]
        +3*rawData[J+1]+6*rawData[J]+3*rawData[J-1]
        +rawData[J+Y+1]+3*rawData[J+Y]+rawData[J+Y-1];
  J := I-Z;
  Zm := rawData[J-Y+1]+3*rawData[J-Y]+rawData[J-Y-1]
        +3*rawData[J+1]+6*rawData[J]+3*rawData[J-1]
        +rawData[J+Y+1]+3*rawData[J+Y]+rawData[J+Y-1];
  result := XYZI (Xm,Xp,Ym,Yp,Zm,Zp,rawData[I]);
  GradMag :=  sqrt( sqr(Xm-Xp)+sqr(Ym-Yp)+sqr(Zm-Zp));//gradient magnitude
end;

procedure NormVol (var Vol: TFloat32s);
var
  n,i: integer;
  mx,mn: single;
begin
  n := length(Vol);
  if n < 1 then
    exit;
  mx := Vol[0];
  mn := Vol[0];
  for i := 0 to (n-1) do begin
    if Vol[i] > mx then
      mx := Vol[i];
    if Vol[i] < mn then
      mn := Vol[i];
  end;
  if mx = mn then
    exit;
  mx := mx-mn;//range
  for i := 0 to (n-1) do
    Vol[i] := (Vol[i]-mn)/mx;
end;

function SmoothVol (var rawData: TUInt8s; lXdim,lYdim,lZdim: integer): integer;
//simple 3D smoothing for noisy data - makes images blurry
//this is useful prior to generating gradients, as it reduces stepping
const
  kCen : single = 2/3; //center of kernel is 2/3
  kEdge : single = 1/6; //voxel on each edge is 1/6
var
   lSmoothImg,lSmoothImg2: TFloat32s;
   lSliceSz,lZPos,lYPos,lX,lY,lZ,lnVox,lVox: integer;
begin
   result := -1;
   lSliceSz := lXdim*lYdim;
   lnVox := lSliceSz*lZDim;
    if (lnVox < 0) or (lXDim < 3) or (lYDim < 3) or (lZDim < 3) then begin
	    showDebug('Smooth3DNII error: Image dimensions are not large enough to filter.');
	    exit;
   end;
  setlength(lSmoothImg,lnVox);
  setlength(lSmoothImg2,lnVox);
  for lX := 0 to (lnVox-1) do
    lSmoothImg[lX] := rawData[lX];
  for lX := 0 to (lnVox-1) do
    lSmoothImg2[lX] := rawData[lX];
  //X-direction - copy from SmoothImg -> SmoothImg2
  for lZ := 2 to lZdim-1 do begin
    lZPos := (lZ-1)*lSliceSz;
    for lY := 2 to lYdim-1 do begin
      lYPos := (lY-1)*lXdim;
      for lX := 2 to lXdim-1 do begin
            lVox := lZPos+lYPos+lX-1;//-1 as indexed from 0
            lSmoothImg2[lVox] := (lSmoothImg[lVox-1]*kEdge)+(lSmoothImg[lVox]*kCen)+(lSmoothImg[lVox+1]*kEdge);
      end; {lX}
    end; {lY}
  end; {lZ loop for X-plane}
  //Y-direction - copy from SmoothImg2 -> SmoothImg
  for lZ := 2 to lZdim-1 do begin
    lZPos := (lZ-1)*lSliceSz;
    for lY := 2 to lYdim-1 do begin
      lYPos := (lY-1)*lXdim;
      for lX := 2 to lXdim-1 do begin
            lVox := lZPos+lYPos+lX-1;//-1 as indexed from 0
            lSmoothImg[lVox] := (lSmoothImg2[lVox-lXdim]*kEdge)+(lSmoothImg2[lVox]*kCen)+(lSmoothImg2[lVox+lXdim]*kEdge);
      end; {lX}
    end; {lY}
  end; {lZ loop for Y-plane}
  //Z-direction - copy from SmoothImg -> SmoothImg2
  for lZ := 2 to lZdim-1 do begin
    lZPos := (lZ-1)*lSliceSz;
    for lY := 2 to lYdim-1 do begin
      lYPos := (lY-1)*lXdim;
      for lX := 2 to lXdim-1 do begin
            lVox := lZPos+lYPos+lX-1;//-1 as indexed from 0
            lSmoothImg2[lVox] := (lSmoothImg[lVox-lSliceSz]*kEdge)+(lSmoothImg[lVox]*kCen)+(lSmoothImg[lVox+lSliceSz]*kEdge);
      end; //x
    end; //y
  end; //z
   //next make this in the range 0..255
  for lX := 0 to (lnVox-1) do
    rawData[lX] := round(lSmoothImg2[lX]);
  lSmoothImg2 := nil;
  lSmoothImg := nil;
end;

procedure CreateGradientVolumeX (rData: TUInt8s; Xsz,Ysz,Zsz, isRGBA: integer; out VolRGBA : TRGBAs);
//compute gradients for each voxel... Output texture in form RGBA
//  RGB will represent as normalized X,Y,Z gradient vector:  Alpha will store gradient magnitude
const
  kEdgeSharpness = 255;//value 1..255: 1=all edges transparent, 255=edges very opaque
var
  X, Y,Z,Index,XYsz : Integer;
  VolData: TUInt8s;
  tRGBA: TRGBAs;
  GradMagS: TFloat32s;
Begin
  tRGBA := nil;
  if (XSz < 1) or (YSz < 1) or (ZSz < 1) then
    exit;
  XYsz :=  Xsz*Ysz;
  Setlength (VolData,XYsz*Zsz);
  if isRGBA = 1 then begin
     tRGBA := TRGBAs(rData );
     for Index := 0 to ((XYsz*Zsz)-1) do
      VolData[Index] := tRGBA[Index].a;
  end else
    for Index := 0 to ((XYsz*Zsz)-1) do
      VolData[Index] := rData[Index];
  //next line: blur the data
  SmoothVol (VolData, Xsz,Ysz,Zsz);
  SetLength (VolRGBA, XYsz*Zsz);
  SetLength (GradMagS,XYsz*Zsz);
  for Index := 0 to ((XYsz*Zsz)-1) do //we can not compute gradients for image edges, so initialize volume so all voxels are transparent
    VolRGBA[Index] := kRGBAclear;
  for Z := 1 To Zsz - 2 do  //for X,Y,Z dimensions indexed from zero, so := 1 gives 1 voxel border
    for Y := 1 To Ysz - 2 do
      for X := 1 To Xsz - 2 do begin
        Index := (Z * XYsz) + (Y * Xsz) + X;
        //Next line computes gradients using Sobel filter
        VolRGBA[Index] := Sobel (VolData, Xsz,Ysz, Index,GradMagS[Index]);
      end;//X
  VolData := nil;
  //next: generate normalized gradient magnitude values
  NormVol (GradMagS);
  for Index := 0 to ((XYsz*Zsz)-1) do
    VolRGBA[Index].A := round(GradMagS[Index]*kEdgeSharpness);
  GradMagS := nil;
end;

procedure CreateGradientVolume (rData: TRGBAs; Xsz,Ysz,Zsz: integer; out VolRGBA : TRGBAs);
begin
     CreateGradientVolumeX (TUInt8s(rData), Xsz,Ysz,Zsz, 1, VolRGBA);
end;

function TNIfTI.GenerateGradientVolume: TRGBAs;
var
  GradRGBA : TRGBAs;
begin
 CreateGradientVolume (fVolRGBA, fDim.X, fDim.Y, fDim.Z, GradRGBA);
 result := GradRGBA;
end;

procedure NII_SetIdentityMatrix (var lHdr: TNIFTIHdr); //create neutral rotation matrix
var lInc: integer;
begin
	with lHdr do begin
		 for lInc := 0 to 3 do
			 srow_x[lInc] := 0;
		 for lInc := 0 to 3 do
             srow_y[lInc] := 0;
         for lInc := 0 to 3 do
             srow_z[lInc] := 0;
         for lInc := 1 to 16 do
             intent_name[lInc] := chr(0);
         //next: create identity matrix: if code is switched on there will not be a problem
		 srow_x[0] := 1;
         srow_y[1] := 1;
         srow_z[2] := 1;
    end;
end; //proc NIFTIhdr_IdentityMatrix

procedure NII_Clear (out lHdr: TNIFTIHdr);
var
 lInc: integer;
begin
  with lHdr do begin
    HdrSz := sizeof(TNIFTIhdr);
    for lInc := 1 to 10 do
       Data_Type[lInc] := chr(0);
    for lInc := 1 to 18 do
       db_name[lInc] := chr(0);
    extents:=0;
    session_error:= 0;
    regular:='r'{chr(0)};
    dim_info:=(0);
    intent_p1 := 0;
    intent_p2 := 0;
    intent_p3 := 0;
    intent_code:=0;
    datatype:=0 ;
    bitpix:=0;
    slice_start:=0;
    for lInc := 1 to 7 do
       pixdim[linc]:= 1.0;
    vox_offset:= 0.0;
    scl_slope := 1.0;
    scl_inter:= 0.0;
    slice_end:= 0;
    slice_code := 0;
    xyzt_units := 10;
    cal_max:= 0.0;
    cal_min:= 0.0;
    slice_duration:=0;
    toffset:= 0;
    glmax:= 0;
    glmin:= 0;
    for lInc := 1 to 80 do
      descrip[lInc] := chr(0);{80 spaces}
    for lInc := 1 to 24 do
      aux_file[lInc] := chr(0);{80 spaces}
    {below are standard settings which are not 0}
    bitpix := 16;//vc16; {8bits per pixel, e.g. unsigned char 136}
    DataType := 4;//vc4;{2=unsigned char, 4=16bit int 136}
    Dim[0] := 3;
    Dim[1] := 256;
    Dim[2] := 256;
    Dim[3] := 1;
    Dim[4] := 1; {n vols}
    Dim[5] := 1;
    Dim[6] := 1;
    Dim[7] := 1;
    glMin := 0;
    glMax := 255;
    qform_code := kNIFTI_XFORM_UNKNOWN;
    sform_code:= kNIFTI_XFORM_UNKNOWN;
    quatern_b := 0;
    quatern_c := 0;
    quatern_d := 0;
    qoffset_x := 0;
    qoffset_y := 0;
    qoffset_z := 0;
    NII_SetIdentityMatrix(lHdr);
    magic := kNIFTI_MAGIC_SEPARATE_HDR;
  end; //with the NIfTI header...
end;

procedure TNIfTI.MakeBorg(voxelsPerDimension: integer);
const
 Border = 4;//margin so we can calculate gradients at edge
var
  d, I, X, Y, Z: integer;
 F: array of single;
 mn, mx, slope: single;
begin
 d := voxelsPerDimension;
 NII_Clear(fhdr);
 fhdr.dim[1] := d;
 fhdr.dim[2] := d;
 fhdr.dim[3] := d;
 fhdr.bitpix := 8;
 fhdr.datatype := kDT_UNSIGNED_CHAR;
 SetLength(F, d * d * d);
 slope := 0.005;
 I := 0;
 for X := 0 to d-1 do
  for Y := 0 to d-1 do
   for Z := 0 to d-1 do
   begin
    if (X < Border) or (Y < Border) or (Z < Border) or ((d-X) < Border) or ((d-Y) < Border) or ((d-Z) < Border) then
     F[I] := 0
    else
     F[I] := sin(slope *x *y) + sin(slope *y * z) + sin(slope *z * x);
    Inc(I);
   end;
 //next find range...
 mn := F[0];
 for I := 0 to d*d*d-1 do
  if F[I] < mn then
   mn := F[I];
 mx := F[0];
 for I := 0 to d*d*d-1 do
  if F[I] > mx then
   mx := F[I];
 slope := 255/(mx-mn);
 SetLength(fRawVolBytes, d*d*d);
 for I := 0 to d*d*d-1 do begin
  if F[I] <= 0 then
   fRawVolBytes[I] := 0
  else
   fRawVolBytes[I] := Round((F[I]-mn)*slope);
 end;
 F := nil;
end;

{$IFDEF GZIP}
function LoadGZ(FileName : AnsiString; var  rawData: TUInt8s; out lHdr: TNIFTIHdr): boolean;// Load 3D data                                 }
//FSL compressed nii.gz file
var
  Stream: TGZFileStream;
begin
 result := false;
 Stream := TGZFileStream.Create (FileName, gzopenread);
 Try
  {$warn 5058 off}Stream.ReadBuffer (lHdr, SizeOf (TNIFTIHdr));{$warn 5058 on}
  if lHdr.HdrSz <> SizeOf (TNIFTIHdr) then begin
   Showdebug('Unable to read image '+Filename+' - this software can only read uncompressed NIfTI files with the same endianess as the host CPU.');
   exit;
  end;
  if (lHdr.bitpix <> 8) and (lHdr.bitpix <> 16) and (lHdr.bitpix <> 24) and (lHdr.bitpix <> 32) then begin
   Showdebug('Unable to load '+Filename+' - this software can only read 8,16,24,32-bit NIfTI files.');
   exit;
  end;
  //read the image data
  Stream.Seek(round(lHdr.vox_offset),soFromBeginning);
  SetLength (rawData, lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3] * (lHdr.bitpix div 8));
  Stream.ReadBuffer (rawData[0], lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3]* (lHdr.bitpix div 8));
 Finally
  Stream.Free;
 End; { Try }
 result := true;
end;
{$ENDIF}

function LoadRaw(FileName : AnsiString; var  rawData: TUInt8s; var lHdr: TNIFTIHdr): boolean;// Load 3D data                                 }
//Uncompressed .nii or .hdr/.img pair
var
 Stream : TFileStream;
begin
 result := false;
 Stream := TFileStream.Create (FileName, fmOpenRead or fmShareDenyWrite);
 Try
  Stream.ReadBuffer (lHdr, SizeOf (TNIFTIHdr));
  if lHdr.HdrSz <> SizeOf (TNIFTIHdr) then begin
   Showdebug('Unable to read image '+Filename+' - this software can only read uncompressed NIfTI files with the same endianess as the host CPU.');
   exit;
  end;
  if (lHdr.bitpix <> 8) and (lHdr.bitpix <> 16) and (lHdr.bitpix <> 24) and (lHdr.bitpix <> 32) then begin
   Showdebug('Unable to load '+Filename+' - this software can only read 8,16,24,32-bit NIfTI files.');
   exit;
  end;
  //read the image data
  if extractfileext(Filename) = '.hdr' then begin
   Stream.Free;
   Stream := TFileStream.Create (changefileext(FileName,'.img'), fmOpenRead or fmShareDenyWrite);
  end;
  Stream.Seek(round(lHdr.vox_offset),soFromBeginning);
  SetLength (rawData, lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3]* (lHdr.bitpix div 8));
  Stream.ReadBuffer (rawData[0], lHdr.Dim[1]*lHdr.Dim[2]*lHdr.Dim[3]* (lHdr.bitpix div 8));
 Finally
  Stream.Free;
 End;
 result := true;
end;

FUNCTION specialsingle (var s:single): boolean;
//returns true if s is Infinity, NAN or Indeterminate
CONST kSpecialExponent = 255 shl 23;
VAR Overlay: LongInt ABSOLUTE s;
BEGIN
 IF ((Overlay AND kSpecialExponent) = kSpecialExponent) THEN
   RESULT := true
 ELSE
   RESULT := false;
END; //specialsingle()

procedure TNIfTI.GPULoadDone();
begin
     fVolRGBA := nil; //close CPU buffer afterit has been copied to GPU
end;

procedure TNIfTI.initUInt8();
//use header's cal_max and cal_min to rescale 8-bit data
var
  histo: array[0..255] of integer;
  i,vx,mn,mx, thresh, sum: integer;
begin
  for i := 0 to 255 do
      histo[i] := 0;
  mn := fRawVolBytes[0];
  mx := mn;
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx-1) do begin
      if fRawVolBytes[i] < mn then
         mn := fRawVolBytes[i];
      if fRawVolBytes[i] > mx then
         mx := fRawVolBytes[i];
      inc(histo[fRawVolBytes[i]]);
  end;
  fMin := (mn * fHdr.scl_slope) + fHdr.scl_inter;
  fMax := (mx * fHdr.scl_slope) + fHdr.scl_inter;
  {$IFDEF UNIX} writeln(format('uint8 range %g...%g', [ fMin, fMax]));{$ENDIF}
  thresh := round(0.01 * vx);
  sum := 0;
  for i := 0 to 255 do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMin := (i * fHdr.scl_slope) + fHdr.scl_inter;
  sum := 0;
  for i := 255 downto 0 do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMax := (i * fHdr.scl_slope) + fHdr.scl_inter;
  {$IFDEF UNIX} writeln(format('uint8 window %g...%g', [ fAutoBalMin, fAutoBalMax])); {$ENDIF}
end;

procedure TNIfTI.convert2Float();
var
  in16,temp16: TUInt16s;
  vol32: TFloat32s;
  i,vx: integer;
begin
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  in16 := TUInt16s(fRawVolBytes);
  setlength(temp16, vx);
  for i := 0 to (vx-1) do
      temp16[i] := in16[i];
  fRawVolBytes := nil; //release
  setlength(fRawVolBytes, 4 * vx);
  vol32 := TFloat32s(fRawVolBytes);
  for i := 0 to (vx-1) do
      vol32[i] := temp16[i];
  fHdr.datatype := kDT_FLOAT32;
  fHdr.bitpix:= 32;
end;

procedure TNIfTI.InitFloat32(); //kDT_FLOAT
const
 kMaxBin = 4095;
var
  vol32: TFloat32s;
  thresh, sum, i,vx: integer;
  mn : Single = 1.0 / 0.0;
  mx : Single = (- 1.0) / (0.0);
  slope: single;
  histo: array[0..kMaxBin] of integer;
begin
  vol32 := TFloat32s(fRawVolBytes);
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx-1) do begin
     if (specialsingle(vol32[i])) then vol32[i] := 0.0;
     vol32[i] := (vol32[i] * fHdr.scl_slope) + fHdr.scl_inter;
     if vol32[i] < mn then
        mn := vol32[i];
     if vol32[i] > mx then
        mx := vol32[i];
  end;
  fMin := mn;
  fMax := mx;
  slope := kMaxBin / (mx - mn) ;
  {$IFDEF UNIX} writeln(format('float range %g..%g',[mn,mx]));{$ENDIF}
  for i := 0 to kMaxBin do
      histo[i] := 0;
  for i := 0 to (vx-1) do
      inc(histo[ round((vol32[i]-mn) * slope)]);
  thresh := round(0.01 * vx);
  sum := 0;
  for i := 0 to kMaxBin do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMin := (i * 1/slope) + mn;
  sum := 0;
  for i := kMaxBin downto 0 do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMax := (i * 1/slope) + mn;
  {$IFDEF UNIX} writeln(format('float window %g...%g', [ fAutoBalMin, fAutoBalMax])); {$ENDIF}
end;

procedure TNIfTI.initInt16(); //kDT_SIGNED_SHORT
const
 kMaxBin = 4095;
var
  vol16: TInt16s;
  histo: array[0..kMaxBin] of integer;
  i,vx,mn,mx, thresh, sum: integer;
  slope: single;
begin
  for i := 0 to kMaxBin do
      histo[i] := 0;
  vol16 := TInt16s(fRawVolBytes);
  mn := vol16[0];
  mx := mn;
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx-1) do begin
      if vol16[i] < mn then
         mn := vol16[i];
      if vol16[i] > mx then
         mx := vol16[i];
  end;
  fMin := (mn * fHdr.scl_slope) + fHdr.scl_inter;
  fMax := (mx * fHdr.scl_slope) + fHdr.scl_inter;
  {$IFDEF UNIX} writeln(format('int16 range %g...%g', [ fMin, fMax]));{$ENDIF}
  slope := kMaxBin / (mx - mn) ;
  for i := 0 to kMaxBin do
      histo[i] := 0;
  for i := 0 to (vx-1) do
      inc(histo[ round((vol16[i]-mn) * slope)]);
  thresh := round(0.01 * vx);
  sum := 0;
  for i := 0 to kMaxBin do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMin := (i * 1/slope) + mn;
  sum := 0;
  for i := kMaxBin downto 0 do begin
      sum := sum + histo[i];
      if (sum > thresh) then break;
  end;
  fAutoBalMax := (i * 1/slope) + mn;
  fAutoBalMin := (fAutoBalMin * fHdr.scl_slope) + fHdr.scl_inter;
  fAutoBalMax := (fAutoBalMax * fHdr.scl_slope) + fHdr.scl_inter;
  {$IFDEF UNIX} writeln(format('int16 window %g...%g', [ fAutoBalMin, fAutoBalMax]));{$ENDIF}
end;

function Scaled2RawIntensity (lHdr: TNIFTIhdr; lScaled: single): single;
begin
  if lHdr.scl_slope = 0 then
	result := (lScaled)-lHdr.scl_inter
  else
	result := (lScaled-lHdr.scl_inter) / lHdr.scl_slope;
end;

procedure TNIfTI.SetDisplayMinMaxInt16();
var
  luts: array[0..255] of TRGBA;
   vol16: TInt16s;
   slope, lMin, lMax: single;
   i, j, vx: integer;
begin
 lMin := Scaled2RawIntensity(fHdr, fWindowMin);
 lMax := Scaled2RawIntensity(fHdr, fWindowMax);
 slope := 255/(lMax - lMin);
  for i := 0 to 255 do
      luts[i] := SetRGBA(i,i,i,i);
  vol16 := TInt16s(fRawVolBytes);
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx - 1) do begin
    if (vol16[i] >= lMax) then
       j := 255
    else if  (vol16[i] <= lMin) then
        j := 0
    else
       j := round((vol16[i] - lMin) * slope);
    fVolRGBA[i] := luts[j];
  end;
end;

procedure TNIfTI.SetDisplayMinMaxFloat32();
var
  luts: array[0..255] of TRGBA;
   vol32: TFloat32s;
   slope: single;
   i, j, vx: integer;
begin
  slope := 255/(fWindowMax - fWindowMin);
  for i := 0 to 255 do
      luts[i] := SetRGBA(i,i,i,i);
  vol32 := TFloat32s(fRawVolBytes);
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx - 1) do begin
    if (vol32[i] >= fWindowMax) then
       j := 255
    else if  (vol32[i] <= fWindowMin) then
        j := 0
    else
       j := round((vol32[i] - fWindowMin) * slope);
    fVolRGBA[i] := luts[j];
  end;
end;

procedure TNIfTI.SetDisplayMinMaxUInt8();
var
  luts: array[0..255] of TRGBA;
  lMin, lMax, lMod, lRng, lSwap: single;
  i, j, vx: integer;
begin
  lMin := Scaled2RawIntensity(fHdr, fWindowMin);
  lMax := Scaled2RawIntensity(fHdr, fWindowMax);
  lRng := (lMax - lMin);
  if lRng <> 0 then
   lMod := abs((((254)/lRng)))
  else
    lMod := 0;
  if lMin > lMax then begin  //maw
       lSwap := lMin;
       lMin := lMax;
       lMax := lSwap;
  end;
  for i := 0 to 255 do begin
    if i <= lMin then
      j := 0
    else if i >= lMax then
        j := 255
    else
       j := trunc(((i-lMin)*lMod)+1);
    luts[i] := SetRGBA(j,j,j,j);
  end;
  vx := (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]);
  for i := 0 to (vx - 1) do
      fVolRGBA[i] := luts[fRawVolBytes[i]];
end;

procedure TNIfTI.SetDisplayMinMax(); overload;
begin
  setlength(fVolRGBA, (fHdr.dim[1]*fHdr.dim[2]*fHdr.dim[3]));
  {$IFDEF UNIX} writeln(format('Window %g..%g', [fWindowMin, fWindowMax])); {$ENDIF}
  if fHdr.datatype = kDT_UINT8 then
     SetDisplayMinMaxUInt8()
  else if fHdr.datatype = kDT_INT16 then
     SetDisplayMinMaxInt16()
  else  if fHdr.datatype = kDT_FLOAT then
     SetDisplayMinMaxFloat32()
  else
      {$IFDEF UNIX} writeln(format('Unsupported data type %d', [fHdr.datatype])){$ENDIF};
end;

procedure TNIfTI.SetDisplayMinMax(newMin, newMax: single); overload;
begin
 fWindowMin := newMin;
 fWindowMax := newMax;
 SetDisplayMinMax;
end;

function TNIfTI.OpenNIfTI(): boolean;
var
   F_Filename: string;
begin
 result := false;
 if (length(fFilename) < 1) or (not FileExists(fFilename)) then exit;
 if uppercase(extractfileext(fFilename)) = '.IMG' then begin
   //NIfTI images can be a single .NII file [contains both header and image]
   //or a pair of files named .HDR and .IMG. If the latter, we want to read the header first
   F_Filename := changefileext(fFilename,'.hdr');
   {$IFDEF LINUX} //LINUX is case sensitive, OSX is not
   Showdebug('Unable to find header (case sensitive!) '+F_Filename);
   {$ELSE}
   Showdebug('Unable to find header '+F_Filename);
   {$ENDIF}
 end else
   F_Filename := fFilename;
 if uppercase(extractfileext(F_Filename)) = '.GZ' then begin
   {$IFDEF GZIP}
   if not LoadGZ(F_FileName, fRawVolBytes, fHdr) then
      exit;
   {$ELSE}
   Showdebug('Please manually decompress images '+F_Filename);
   exit;
   {$ENDIF}
 end else begin
   if not LoadRaw(F_FileName, fRawVolBytes, fHdr) then
    exit;
 end;
 result := true;
end;

procedure TNIfTI.Load(niftiFileName: string);
var
   scaleMx: single;
begin
      fFilename := niftiFileName;
      if not OpenNIfTI() then
         MakeBorg(64);
      scaleMx := max(max(abs(fHdr.Dim[1]*fHdr.PixDim[1]),abs(fHdr.Dim[2]*fHdr.pixDim[2])),abs(fHdr.Dim[3]*fHdr.pixDim[3]));
      if (scaleMx <> 0) then begin
        fScale.X := abs((fHdr.Dim[1]*fHdr.PixDim[1]) / scaleMx);
        fScale.Y := abs((fHdr.Dim[2]*fHdr.PixDim[2]) / scaleMx);
        fScale.Z := abs((fHdr.Dim[3]*fHdr.PixDim[3]) / scaleMx);
      end;
      fDim.x := fHdr.Dim[1];
      fDim.y := fHdr.Dim[2];
      fDim.z := fHdr.Dim[3];
      if fHdr.scl_slope = 0 then fHdr.scl_slope := 1;
      if fHdr.datatype = kDT_UINT16 then
         convert2Float();
      if fHdr.datatype = kDT_UINT8 then
         initUInt8()
      else if fHdr.datatype = kDT_INT16 then
           initInt16()
      else if fHdr.datatype = kDT_FLOAT then
           initFloat32()
      else
          {$IFDEF UNIX} writeln('Unsupported data format '+inttostr(fHdr.datatype)){$ENDIF};
      fWindowMin := fAutoBalMin;
      fWindowMax := fAutoBalMax;
      if (fHdr.cal_max > fHdr.cal_min) then begin
         fWindowMin := fHdr.cal_min;
         fWindowMax := fHdr.cal_max;
      end;
      SetDisplayMinMax();
end;

constructor TNIfTI.Create(niftiFileName: string); overload;
begin
     fRawVolBytes := nil;
     Load(niftiFileName);
end;

constructor TNIfTI.Create(); overload;
begin
     Create('');
end;

destructor  TNIfTI.Destroy;
begin
     if fRawVolBytes = nil then
        freeandnil(fRawVolBytes);
end;

end.
