unit mesh;

{$mode objfpc}{$H+}
//read OBJ/PLY format files
interface

uses
  VectorMath, SimdUtils, Classes, SysUtils, dialogs, zstream, math;

type
TPoint3f = TVec3;
TPoint3i = TVec3i;
TFaces = array of TPoint3i;
TVertices = array of TPoint3f;
TVertexRGBA = array of TRGBA;
procedure LoadMesh(Filename: string; var faces: TFaces; var vertices: TVertices; var vertexRGBA: TVertexRGBA); overload;
procedure LoadMesh(Filename: string; var faces: TFaces; var vertices, normals: TVertices; var vertexRGBA: TVertexRGBA; defaultColor: TRGBA; isSwapYZ: boolean = true); overload;
procedure SaveObj(const FileName: string; var faces: TFaces; var vertices: TVertices);

implementation

function getSurfaceNormal(v1, v2, v3: TPoint3f): TPoint3f;
var
   polyVector1, polyVector2: TPoint3f;
begin
 polyVector1 := Vec3(v2.x - v1.x, v2.y - v1.y, v2.z - v1.z);
 polyVector2 := Vec3(v3.x - v1.x, v3.y - v1.y, v3.z - v1.z);
 //result := crossProduct(polyVector1, polyVector2);
 result :=  polyVector1.Cross(polyVector2);
 //result := result.Normalize;
 //make sure to eventually normalize the result!
end; // getSurfaceNormal()

procedure SaveObj(const FileName: string; var faces: TFaces; var vertices: TVertices);
//create WaveFront object file
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
var
   f : TextFile;
   FileNameObj: string;
   i : integer;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     showmessage('You need to open a mesh before you can save it');
     exit;
  end;
  FileNameObj := changeFileExt(FileName, '.obj');
  AssignFile(f, FileNameObj);
  ReWrite(f);
  WriteLn(f, '# WaveFront Object format image created with Surf Ice');
  for i := 0 to (length(vertices)-1) do
      WriteLn(f, 'v ' + floattostr(vertices[i].X)+' '+floattostr(vertices[i].Y)+' '+ floattostr(vertices[i].Z));
  for i := 0 to (length(faces)-1) do
      WriteLn(f, 'f ' + inttostr(faces[i].X+1)+' '+inttostr(faces[i].Y+1)+' '+ inttostr(faces[i].Z+1)); //+1 since "A valid vertex index starts from 1 "
  //fprintf(fid, '# WaveFront Object format image created with MRIcroS\n');
  //fprintf(fid, 'v %.12g %.12g %.12g\n', vertex');
  //fprintf(fid, 'f %d %d %d\n', (face)');
  CloseFile(f);
end; // SaveObj()

procedure SwapLongWord(var s : LongWord);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongWord);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s := outguy.Long;
end; // SwapLongWord()

function asSingle(i : longint): single;  overload;
type
  swaptype = packed record
    case byte of
      0:(Lng : longint);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
  inguy := @i; //assign address of s to inguy
  result := inguy^.Sngl;
end; // asSingle()

function asSingle(b0,b1,b2,b3: byte): single; overload;
type
  swaptype = packed record
    case byte of
      0:(b0,b1,b2,b3 : byte);
      1:(Sngl : single);
  end;
var
  outguy:swaptype;
begin //should work with both little and big endian, as order is same
  outguy.b0 := b0;
  outguy.b1 := b1;
  outguy.b2 := b2;
  outguy.b3 := b3;
  result := outguy.Sngl;
end; // asSingle()

procedure SwapSingle(var s : single);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s := outguy.Sngl;
end; // SwapSingle()

function FSize (lFName: String): longint;
var F : File Of byte;
begin
  result := 0;
  if not fileexists(lFName) then exit;
  Assign (F, lFName);
  Reset (F);
  result := FileSize(F);
  Close (F);
end;

function LoadMz3Core(const FileName: string; var Faces: TFaces; var Vertices: TVertices; var vertexRGBA: TVertexRGBA): boolean;
const
 kMagic =  23117; //"MZ"
 kChunkSize = 16384;
label 666;
var
  i: integer;
  bytes : array of byte;
  Magic, Attr: uint16;
  nFace, nVert, nSkip: uint32;
  isFace, isVert, isRGBA, isScalar: boolean;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
begin
     result := false;
     setlength(Faces,0);
     setlength(Vertices,0);
     setlength(vertexRGBA,0);
     if not fileexists(Filename) then exit;
     mStream := TMemoryStream.Create;
     zStream := TGZFileStream.create(FileName, gzopenread);
     setlength(bytes, kChunkSize);
     repeat
            i := zStream.read(bytes[0],kChunkSize);
            mStream.Write(bytes[0],i) ;
     until i < kChunkSize;
     zStream.Free;
     if mStream.Size < 28 then begin
       //showmessage('MZ3 file too small'+inttostr(mStream.Size));
       exit; //16 byte header, 3 vertices, single 4-byte scalar per vertex vertices
     end;
     mStream.Position := 0;
     mStream.Read(Magic,2);
     mStream.Read(Attr,2);
     mStream.Read(nFace,4);
     mStream.Read(nVert,4);
     mStream.Read(nSkip,4);
     if (magic <> kMagic) then goto 666;
     isFace := (Attr and 1) > 0;
     isVert := (Attr and 2) > 0;
     isRGBA := (Attr and 4) > 0;
     isScalar := (Attr and 8) > 0;
     if (Attr > 15) then begin
        showmessage('Unsupported future format '+ inttostr(Attr));
        goto 666;
     end;
     if (nFace = 0) and (isFace) then goto 666;
     if (nVert = 0) and ((isVert) or (isRGBA) or (isScalar) ) then goto 666;
     if nSkip > 0 then
        mStream.Seek(nSkip, soFromCurrent);
     result := true;
     if isFace then begin
        setlength(Faces,  nFace);
        mStream.Read(Faces[0], nFace * 3 * sizeof(int32));
     end;
     if isVert then begin
        setlength(Vertices,  nVert);
        mStream.Read(Vertices[0], nVert * 3 * sizeof(single));
     end;
     if isRGBA then begin
        setlength(vertexRGBA, nVert);
        mStream.Read(vertexRGBA[0], nVert * 4 * sizeof(byte));
        if isScalar then
           for i := 1 to (nVert -1) do
               vertexRGBA[i].A := 255;
     end;
     if isScalar then begin
        writeln('Scalar intensity MZ3 not supported');
        //setlength(intensity, nVert);
        //mStream.Read(intensity[0], nVert * sizeof(single));
     end;
     if (isRGBA) and (isScalar) then begin //atlas template the float "intensity" stores integer of index
        writeln('Atlas MZ3 not supported');
     end;
     result := true;
   666 :
     mStream.Free;
end; // LoadMz3Core()


procedure LoadObj(const FileName: string; var faces: TFaces; var vertices: TVertices);//; var vertexRGBA: TVertexRGBA);
//WaveFront Obj file used by Blender
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
const
  kBlockSize = 8192;
var
   f: TextFile;
   fsz : int64;
   s : string;
   strlst : TStringList;
   i,j, num_v, num_f, new_f: integer;
begin
     fsz := FSize (FileName);
     if fsz < 32 then exit;
     //init values
     num_v := 0;
     num_f := 0;
     strlst:=TStringList.Create;
     setlength(vertices, (fsz div 70)+kBlockSize); //guess number of faces based on filesize to reduce reallocation frequencey
     setlength(faces, (fsz div 35)+kBlockSize); //guess number of vertices based on filesize to reduce reallocation frequencey
     //load faces and vertices
     AssignFile(f, FileName);
     Reset(f);
     DefaultFormatSettings.DecimalSeparator := '.';
     while not EOF(f) do begin
        readln(f,s);
        if length(s) < 7 then continue;
        if (s[1] <> 'v') and (s[1] <> 'f') then continue; //only read 'f'ace and 'v'ertex lines
        if (s[2] = 'p') or (s[2] = 'n') or (s[2] = 't') then continue; //ignore vp/vn/vt data: avoid delimiting text yields 20% faster loads
        strlst.DelimitedText := s;
        if (strlst.count > 3) and ( (strlst[0]) = 'f') then begin
           //warning: need to handle "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3"
           //warning: face could be triangle, quad, or more vertices!
           new_f := strlst.count - 3;
           if ((num_f+new_f) >= length(faces)) then
              setlength(faces, length(faces)+new_f+kBlockSize);
           for i := 1 to (strlst.count-1) do
               if (pos('/', strlst[i]) > 1) then // "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3" -> f v1 v2 v3
                  strlst[i] := Copy(strlst[i], 1, pos('/', strlst[i])-1);
           for j := 1 to (new_f) do begin
               faces[num_f].X := strtointDef(strlst[1], 0) - 1;
               faces[num_f].Y := strtointDef(strlst[j+1], 0) - 1;  //-1 since "A valid vertex index starts from 1"
               faces[num_f].Z := strtointDef(strlst[j+2], 0) - 1;  //-1 since "A valid vertex index starts from 1"
               inc(num_f);
           end;
        end;
        if (strlst.count > 3) and ( (strlst[0]) = 'v') then begin
           if ((num_v+1) >= length(vertices)) then
              setlength(vertices, length(vertices)+kBlockSize);
           vertices[num_v].X := strtofloatDef(strlst[1], 0);
           vertices[num_v].Y := strtofloatDef(strlst[2], 0);
           vertices[num_v].Z := strtofloatDef(strlst[3], 0);
           inc(num_v);
        end;
     end;
     CloseFile(f);
     strlst.free;
     setlength(faces, num_f);
     setlength(vertices, num_v);
end; // LoadObj()

procedure LoadPly(const FileName: string; var faces: TFaces; var vertices: TVertices; var vertexRGBA: TVertexRGBA);
// https://en.wikipedia.org/wiki/PLY_(file_format)
// http://paulbourke.net/dataformats/ply/
var
   fb: file;
   f: TextFile;
   isSwap, isVertexSection, isAscii, isLittleEndian, isUint32 : boolean;
   redOffset, greenOffset, blueOffset, AlphaOffset,
   hdrSz, sz, i, j,  num_v, num_f, num_vx, num_header_lines, vertexOffset, indexSectionExtraBytes: integer;
   str: string;
   byt: byte;
   flt: single;
   strlst : TStringList;
   i32: array [1..3] of longword;
   i16: array [1..3] of word;
   binByt: array of byte;
begin
  AssignFile(f, FileName);
  Reset(f);
  ReadLn(f, str);
  if pos('PLY', UpperCase(str)) <> 1 then begin
    showmessage('Not a PLY file');
    closefile(f);
    exit;
  end;
  strlst:=TStringList.Create;
  num_header_lines := 1;
  num_f := 0;
  num_v := 0;
  isAscii := false;
  isLittleEndian := false;
  isUint32 := true; //assume uint32 not short int16
  isVertexSection := false;
  indexSectionExtraBytes := 0;
  vertexOffset := 0;
  redOffset := 0; greenOffset := 0; blueOffset := 0; AlphaOffset := 0;
  while not EOF(f) do begin
     ReadLn(f, str);
     num_header_lines := num_header_lines + 1;
     if pos('END_HEADER', UpperCase(str)) = 1 then Break;
     if pos('FORMAT', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        if pos('ASCII', UpperCase(strlst[1])) = 1 then
            isAscii := true;
        if pos('BINARY_LITTLE_ENDIAN', UpperCase(strlst[1])) = 1 then
            isLittleEndian := true;
     end;
     if pos('ELEMENT VERTEX', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        num_v := StrToIntDef(strlst[2], 0); // "element vertex 62"
        isVertexSection := true;
     end;
     if pos('ELEMENT FACE', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        num_f := StrToIntDef(strlst[2], 0); // "element face 120"
        isVertexSection := false;
     end;
     //detect "short" or "uint" from "property list uchar uint vertex_indices"

     if (isVertexSection) and (pos('PROPERTY', UpperCase(str)) = 1) then begin
        strlst.DelimitedText := str;
        if (strlst.count > 2) and (pos('RED', UpperCase(strlst[2])) = 1) then begin
           redOffset := vertexOffset;
           if (pos('UCHAR', UpperCase(strlst[1])) <> 1) then begin
             showmessage('Expected colors of data type "UCHAR", not "'+str+'"');
             closefile(f);
             exit;
           end;
        end;
        if (strlst.count > 2) and (pos('GREEN', UpperCase(strlst[2])) = 1) then
           greenOffset := vertexOffset;
        if (strlst.count > 2) and (pos('BLUE', UpperCase(strlst[2])) = 1) then
           blueOffset := vertexOffset;
        if (strlst.count > 2) and (pos('ALPHA', UpperCase(strlst[2])) = 1) then
           alphaOffset := vertexOffset;
        //showmessage(str+ inttostr(strlst.count));
        if isAscii then
          vertexOffset := vertexOffset + 1  //for ASCII we count items not bytes
        else if (pos('CHAR', UpperCase(strlst[1])) = 1) or (pos('UCHAR', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 1
        else if (pos('SHORT', UpperCase(strlst[1])) = 1) or (pos('USHORT', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 2
        else if (pos('INT', UpperCase(strlst[1])) = 1) or (pos('UINT', UpperCase(strlst[1])) = 1) or (pos('FLOAT', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 4
        else if (pos('DOUBLE', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 8
        else begin
            showmessage('Unexpected data type : "'+UpperCase(strlst[1])+'"');
            closefile(f);
            exit;
        end;
     end; //Vertex section properties
     if (not isVertexSection) and (pos('PROPERTY', UpperCase(str)) = 1) then begin
        //n.b. Wiki and MeshLab use  'VERTEX_INDICES' but Bourke uses "VERTEX_INDEX"
        strlst.DelimitedText := str;
        if (strlst.count > 4) and (pos('VERTEX_INDEX', UpperCase(strlst[4])) = 1) and (pos('SHORT', UpperCase(strlst[3])) = 1) then
           isUint32 := false
        else if (strlst.count > 4) and (pos('VERTEX_INDICES', UpperCase(strlst[4])) = 1) and (pos('SHORT', UpperCase(strlst[3])) = 1) then
           isUint32 := false
        else if (strlst.count > 2)  then begin
           if (pos('CHAR', UpperCase(strlst[1])) = 1) or  (pos('UCHAR', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 1;
           if (pos('SHORT', UpperCase(strlst[1])) = 1) or  (pos('USHORT', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 2;
           if (pos('INT', UpperCase(strlst[1])) = 1) or  (pos('UINT', UpperCase(strlst[1])) = 1)  or  (pos('FLOAT', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 4;
           if (pos('DOUBLE', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 8;
        end;
     end; //face section properties

  end;
  if EOF(f) or (num_v < 3) or (num_f < 1) then begin
    showmessage('Not a mesh-based PLY file (perhaps point based, try opening in MeshLab)');
    closefile(f);
    exit;
  end;

  setlength(vertices, num_v);
  setlength(faces, num_f);
  if redOffset > 2 then
     setlength(vertexRGBA,num_v);
  if isAscii then begin
    if redOffset > 2 then begin
       sz := redOffset;
       if (greenOffset > sz) then sz := greenOffset;
       if (blueOffset > sz) then sz := blueOffset;
       if (alphaOffset > sz) then sz := alphaOffset;
       for i := 0 to (num_v - 1) do begin
           read(f, vertices[i].X, vertices[i].Y, vertices[i].Z);  //XYZ are items 0,1,2
           for j := 3 to (sz) do begin
               read(f, flt);
               if j = redOffset then vertexRGBA[i].R := round(flt);
               if j = greenOffset then vertexRGBA[i].G := round(flt);
               if j = blueOffset then vertexRGBA[i].B := round(flt);
               if j = alphaOffset then vertexRGBA[i].A := round(flt);
           end;
           readln(f);
       end;
    end else
        for i := 0 to (num_v - 1) do
            readln(f, vertices[i].X, vertices[i].Y, vertices[i].Z);
    for i := 0 to (num_f - 1) do begin
      readln(f, num_vx, faces[i].X, faces[i].Y, faces[i].Z);
      if num_vx < 3 then begin
            showmessage('File does not have the expected number of triangle-based faces '+ FileName);
            closefile(f);
            exit;
      end;
      if num_vx > 3 then begin
          showmessage('Only able to read triangle-based PLY files. (Hint: open with MeshLab and export as CTM format) ');
          closefile(f);
          exit;
      end;
    end;
    closefile(f);
  end else begin //if ASCII else Binary
    closefile(f);
    isSwap := false;
    {$IFDEF ENDIAN_LITTLE}
    if not isLittleEndian then begin
    {$ELSE}
    if isLittleEndian then begin
    {$ENDIF}
      //showmessage('unsupported binary PLY feature: swapped bytes');
      //exit;
      isSwap := true;
    end;
    if vertexOffset < 12 then begin
       showmessage('Binary PLY files should have at least 12 bytes per vertex');
       exit;
    end;
    AssignFile(fb, FileName);
    FileMode := fmOpenRead;
    Reset(fb,1);
    num_vx := 0;
    sz := filesize(fb);
    i := 0;
    while (num_vx < num_header_lines) and (i < sz) do begin
          blockread(fb, byt, 1 );
          if byt = $0A then
             num_vx := num_vx + 1;
          i := i + 1;
    end;
    hdrSz := i;
    if (num_vx < num_header_lines) then begin
       closefile(fb);
       exit;
    end;
    if vertexOffset > 12 then begin
       setlength(binByt, vertexOffset);
       for i := 0 to (num_v -1) do begin
           blockread(fb, binByt[0], vertexOffset );//sizeof(clrV) );
           vertices[i].X := asSingle(binByt[0],binByt[1],binByt[2],binByt[3]);
           vertices[i].Y := asSingle(binByt[4],binByt[5],binByt[6],binByt[7]);
           vertices[i].Z := asSingle(binByt[8],binByt[9],binByt[10],binByt[11]);
           if redOffset > 0 then begin
              vertexRGBA[i].R := binByt[redOffset];
              vertexRGBA[i].G := binByt[greenOffset];
              vertexRGBA[i].B := binByt[blueOffset];
              if alphaOffset > 0 then
                 vertexRGBA[i].A := binByt[alphaOffset];
           end;
       end;
       i := 0;
    end else
        blockread(fb, vertices[0], 3 * 4 * num_v);
    if isSwap then begin
          for i := 0 to (num_v -1) do begin
              SwapSingle(vertices[i].X);
              SwapSingle(vertices[i].Y);
              SwapSingle(vertices[i].Z);
          end;
    end; //swapped
    for i := 0 to (num_f -1) do begin
        setlength(binByt, indexSectionExtraBytes);
        blockread(fb, byt, 1 );
        if byt <> 3 then begin
                showmessage('Only able to read triangle-based PLY files. Solution: open and export with MeshLab. Index: '+inttostr(i)+ ' Header Bytes '+inttostr(hdrSz)+' bytesPerVertex: '+inttostr(vertexOffset)+' faces: ' + inttostr(byt));
                closefile(fb);
                setlength(faces,0);
                setlength(vertices,0);
                exit;
        end;
        if isSwap then begin
           if isUint32 then begin
              blockread(fb, i32[1], 3 * 4 );
              SwapLongWord(i32[1]);
              SwapLongWord(i32[2]);
              SwapLongWord(i32[3]);
              faces[i] := pti(i32[1], i32[2], i32[3]);  //winding order matches MeshLab
           end else begin
               blockread(fb, i16[1], 3 * 2 );
               faces[i] := pti(swap(i16[1]), swap(i16[2]), swap(i16[3])); //winding order matches MeshLab
           end;
        end else begin
          if isUint32 then begin
             blockread(fb, i32[1], 3 * 4 );
             faces[i] := pti(i32[1], i32[2], i32[3]);  //winding order matches MeshLab
          end else begin
              blockread(fb, i16[1], 3 * 2 );
              faces[i] := pti(i16[1], i16[2], i16[3]); //winding order matches MeshLab
          end;

        end; //is Swapped else unSwapped
        if (indexSectionExtraBytes > 0) then
           blockread(fb, binByt[0], indexSectionExtraBytes);
    end;
    closefile(fb);
   end; //if ascii else binary
   strlst.Free;
end; // LoadPly()

procedure minMax(var v, mn, mx: TPoint3f);
begin
     if v.X > mx.X then
        mx.X := v.X
     else if v.X < mn.X then
        mn.X := v.X;
     if v.Y > mx.Y then
        mx.Y := v.Y
     else if v.Y < mn.Y then
        mn.Y := v.Y;
     if v.Z > mx.Z then
        mx.Z := v.Z
     else if v.Z < mn.Z then
        mn.Z := v.Z;
end; // minMax()

function SetDescriptives (var vertices: TVertices; out origin: TPoint3f): single;
//determine range of vertices in each dimension
var
   mn, mx: TPoint3f;
   Scale: single;
   i: integer;
begin
     if length(vertices) < 1 then exit;
     mx := vertices[0];
     mn := mx;
     for i := 0 to (length(vertices) - 1) do
         minMax(vertices[i], mn, mx);

     origin.X := (0.5 * (mx.X - mn.X)) + mn.X;
     origin.Y := (0.5 * (mx.Y - mn.Y)) + mn.Y;
     origin.Z := (0.5 * (mx.Z - mn.Z)) + mn.Z;
     Scale := abs(mx.X - origin.X);
     if abs(mx.Y - origin.Y) > Scale then
        Scale := abs(mx.Y - origin.Y);
     if abs(mx.Z - origin.Z) > Scale then
        Scale := abs(mx.Z - origin.Z);
     result := Scale;
end; // SetDescriptives()

procedure NormalizeMeshSize(var vertices: TVertices);
//make size mesh -1..1 in largest dimension
var
   i: integer;
   scale : single;
   origin: TPoint3f;
begin
  if (length(vertices) < 3) then exit;
  scale := SetDescriptives (vertices, origin);
  if scale = 0 then
     exit;
  for i := 0 to (length(vertices)-1) do begin
     vertices[i].X := (vertices[i].X - origin.X)/ scale;
     vertices[i].Y := (vertices[i].Y - origin.Y)/ scale;
     vertices[i].Z := (vertices[i].Z - origin.Z)/ scale;
  end;
end; //NormalizeMeshSize()

//
function  getFirstPerpVector(v1: TVec3): TVec3;
//https://stackoverflow.com/questions/1878257/how-can-i-draw-a-cylinder-that-connects-two-points-in-opengl
//return a vector that is perpendicular to the first
begin
 result := Vec3(0.0,0.0,0.0);
 if ((v1.x = 0.0) or (v1.y = 0.0) or (v1.z = 0.0)) then begin
   if (v1.x = 0.0) then
     result.x := 1.0
   else if (v1.y = 0.0) then
     result.y := 1.0
   else
     result.z := 1.0;
 end else begin
   // If xyz is all set, we set the z coordinate as first and second argument .
   // As the scalar product must be zero, we add the negated sum of x and y as third argument
   result.x := v1.z;      //scalp = z*x
   result.y := v1.z;      //scalp = z*(x+y)
   result.z := -(v1.x+v1.y); //scalp = z*(x+y)-z*(x+y) = 0
   // Normalize vector
   result := result.Normalize;
 end;
end;

procedure makeCylinder(radius: single; start, dest: TVec3; var faces: TFaces; var vertices: TVertices; sides: integer = 20); overload;
//https://stackoverflow.com/questions/1878257/how-can-i-draw-a-cylinder-that-connects-two-points-in-opengl
{$DEFINE ENDCAPS}
var
	v1, v2, v3, pt: TVec3;
    c, s: single;
    i, num_v, num_f: integer;
begin
    if (sides < 3) then sides := 3; //prism is minimal 3D cylinder
  	v1 := (dest - start).Normalize; //principle axis of cylinder
    v2 := getFirstPerpVector(v1); //a unit length vector orthogonal to v1
    // Get the second perp vector by cross product
    v3 := (v1.Cross(v2)).Normalize; //a unit length vector orthogonal to v1 and v2
    num_v := 2 * sides;
    num_f := 2 * sides;
    {$IFDEF ENDCAPS}
    num_f += 2 * (sides - 2); //a prism endcap is one triangle, hexagonal has 4, etc.
    {$ENDIF}
    setlength(faces, num_f);
    setlength(vertices, num_v);
    for i := 0 to (sides-1) do begin
      c :=  cos(i/sides * 2 * PI);
      s :=  sin(i/sides * 2 * PI);
      pt.x := (radius * (c * v2.x+ s *v3.x));
      pt.y := (radius * (c * v2.y+ s *v3.y));
      pt.z := (radius * (c * v2.z+ s *v3.z));
      vertices[i] := start+pt;
      vertices[i + sides] := dest+pt;
      if i < (sides-1) then begin
        faces[i * 2] := pti( i,  i + 1, i + sides);
        faces[(i * 2)+1] := pti( i + 1,  i + sides + 1, i + sides);
      end else begin //final 2 triangles of cylinder share vertices with first triangle (close the loop)
        faces[i * 2] := pti( i,  0, i + sides);
        faces[i * 2 + 1] := pti( 0,  0 + sides, i + sides);
      end;
      {$IFDEF ENDCAPS}
      if i < 2 then continue; //a prism endcap is one triangle, hexagonal has 4, etc.
      faces[(sides*2)+(i-2)] := pti(i, 0, i - 1);
      faces[(sides*2)+(i-2)+(sides-2)] := pti(i+ sides, 0+ sides, i - 1+ sides);
      {$ENDIF}
    end;
end;

procedure AddCylinder(radius: single; start, dest: TVec3; var faces: TFaces; var vertices: TVertices);
var
    f: TFaces;
    v: TVertices;
    i, n: integer;
begin
	makeCylinder(radius, start, dest, f, v);
    if length(vertices) > 0 then begin
    	n := length(vertices);
    	for i := 0 to (length(f)-1) do
            f[i] := pti(f[i].x + n, f[i].y + n, f[i].z + n);
    end;
    faces := Concat(faces, f);
    vertices := Concat(vertices, v);
end;

procedure MakeCrosshair(var faces: TFaces; var vertices: TVertices);
var
    radius: single = 0.05;
    sliceFrac: TVec3; //location of crosshairs, 0..1 in each dimension
begin
  sliceFrac := Vec3(0.5, 0.5, 0.5);
  AddCylinder(radius, Vec3(sliceFrac.x, sliceFrac.y, -0.1), Vec3(sliceFrac.x, sliceFrac.y, 1.1), faces, vertices);
  AddCylinder(radius, Vec3(sliceFrac.x, -0.1, sliceFrac.z),  Vec3(sliceFrac.x, 1.1, sliceFrac.z), faces, vertices);
  AddCylinder(radius, Vec3(-0.1, sliceFrac.y, sliceFrac.z),  Vec3(1.1, sliceFrac.y, sliceFrac.z), faces, vertices);
end;

procedure MakeTrefoil(var faces: TFaces; var vertices: TVertices);
//http://prideout.net/blog/?p=22
function EvaluateTrefoil(s, t: single): TPoint3f;
const
     TwoPi = 2 * Pi;
    a = 0.5;
    b = 0.3;
    c = 0.5;
    d = 0.1;
var
   u,v,r,x,y,z: single;
   q, qvn, ww, dv : TPoint3f;
begin
    u := (1 - s) * 2 * TwoPi;
    v := t * TwoPi;
    r := a + b * cos(1.5 * u);
    x := r * cos(u);
    y := r * sin(u);
    z := c * sin(1.5 * u);
    dv := Vec3(-1.5 * b * sin(1.5 * u) * cos(u) -  (a + b * cos(1.5 * u)) * sin(u),
        -1.5 * b * sin(1.5 * u) * sin(u) + (a + b * cos(1.5 * u)) * cos(u),
         1.5 * c * cos(1.5 * u) );
    q := dv.Normalize;
    qvn.X := q.y;
    qvn.Y := -q.X;
    qvn.z := 0;
    qvn := qvn.Normalize;
    ww := q.Cross(qvn);
    result := Vec3(x + d * (qvn.x * cos(v) + ww.x * sin(v)),
            y + d * (qvn.y * cos(v) + ww.y * sin(v)),
            z + d * ww.z * sin(v));
end;
const
  kSlices = 128;
  kStacks = 32;
  kVertexCount = kSlices * kStacks;
  kIndexCount = kVertexCount * 6;
var
   ds, dt, s, t: single;
   i,j,k, n: integer;
begin
  setlength(faces, kIndexCount);
  n := 0;
  k := 0;
  for i := 0 to (kSlices-1) do begin
    for j := 0 to (kStacks-1) do begin
        faces[k].x := n + j;
        faces[k].z := n + (j + 1) mod kStacks;
        faces[k].y := (n + j + kStacks) mod kVertexCount;
        k := k + 1;
        faces[k].x := (n + j + kStacks) mod kVertexCount;
        faces[k].z := (n + (j + 1) mod kStacks) mod kVertexCount;
        faces[k].y := (n + (j + 1) mod kStacks + kStacks) mod kVertexCount;
        k := k + 1;
      end;
      n := n + kStacks;
    end;
  setlength(vertices, kVertexCount);
  setlength(faces, kIndexCount);
  ds := 1.0 / kSlices;
  dt := 1.0 / kStacks;
  // The upper bounds in these loops are tweaked to reduce the
  // chance of precision error causing an incorrect # of iterations.
  s := 0.0;
  i := 0;
  while (s < 1 - ds / 2) do begin
    s := s + ds;
    t := 0;
    while (t < 1 - dt / 2) do begin
      t := t + dt;
      vertices[i] := EvaluateTrefoil(s, t);
      i := i+1;
    end;
  end;
end; // MakeTrefoil()

procedure LoadMesh(Filename: string; var faces: TFaces; var vertices: TVertices; var vertexRGBA: TVertexRGBA);
var
   Ext: string;
begin
  setlength(faces, 0);
  setlength(vertices,0);
  setlength(vertexRGBA, 0);
  if (length(Filename) > 0) and (FileExists(Filename)) and (not DirectoryExists(Filename)) and (FSize(FileName) > 9) then begin;
    Ext := UpperCase(ExtractFileExt(Filename));
    if (Ext = '.MZ3') then
       LoadMz3Core(Filename, faces, vertices, vertexRGBA)
    else if (Ext = '.OBJ') then
           LoadObj(Filename, faces, vertices)
    else
       LoadPly(Filename, faces, vertices, vertexRGBA);
  end;
  if (length(faces) < 1) or (length(vertices) < 3) then
      MakeCrossHair(faces, vertices);//MakeTrefoil(faces, vertices);
  NormalizeMeshSize(vertices);
end; //LoadMesh()

procedure LoadMesh(Filename: string; var faces: TFaces; var vertices, normals: TVertices; var vertexRGBA: TVertexRGBA; defaultColor: TRGBA; isSwapYZ: boolean = true); overload;
var
   i: integer;
   swap: TPoint3f;
   fNorm: TPoint3f;
begin
  LoadMesh(Filename, faces, vertices, vertexRGBA);
  if (isSwapYZ) then begin
     for i := 0 to (length(vertices) - 1) do begin
      swap.X := vertices[i].X;
      swap.Y := -vertices[i].Z;
      swap.Z := vertices[i].Y;
      vertices[i] := swap;
     end;
   end;
  setlength(normals, length(vertices));
  fNorm := Vec3(0,0,0);
  for i := 0 to (length(vertices)-1) do
      normals[i] := fNorm;
  for i := 0 to (length(faces)-1) do begin //compute the normal for each face
      fNorm := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
      normals[faces[i].X] += fNorm;
      normals[faces[i].Y] += fNorm;
      normals[faces[i].Z] += fNorm;
  end;
  for i := 0 to (length(vertices)-1) do
      normals[i] := normals[i].normalize;
  if length(vertexRGBA) < length(vertices) then begin
     setlength(vertexRGBA, length(vertices));
     vertexRGBA[0] := defaultColor;
     for i := 0 to (length(vertices)-1) do
         vertexRGBA[i] := vertexRGBA[0];
  end;
end; //loadMesh()

end.

