unit SimdUtils;
interface
{$IFDEF Darwin}
  //{$mode objfpc}
  {$modeswitch objectivec1}
{$ENDIF}
uses
  {$IFDEF Darwin} CocoaAll, MacOSAll, {$ENDIF}
  sysutils, dialogs;


const
  kMaxUniform = 12;//for Metal: divisible by 4
type
 TUniform = record
   Name,Hint: string;
   Widget: integer;
   Min,DefaultV,Max: single;
   Bool: boolean;
 end;
 TShaderPrefs = record
        nUniform: integer;
        Uniform: array [1..kMaxUniform] of TUniform;
 end;
  TUInt32s = array of uint32;
  TInt32s = array of int32;
  TUInt16s = array of uint16;
  TInt16s = array of int16;
  TUInt8s = array of uint8;
  TInt8s = array of int8;
  TFloat32s = array of single;
  TFloat64s = array of double;
  TRGB = packed record //red,green,blue
   R,G,B : byte;
 end;
 TRGBs = array of TRGB;
 TRGBA = packed record //red,green,blue,alpha
   R,G,B,A : byte;
 end;
 TRGBAs = array of TRGBA;
 TLUT = array [0..255] of TRGBA; //Color Lookup Table
 TVec3i = packed record
         case integer of
	    0: (v: array[0..2] of int32);
	    1: (x, y, z: int32);
         end;
 TVec4i = packed record
         case integer of
	    0: (v: array[0..3] of int32);
	    1: (x, y, z, t: int32);
         end;
 TVec6i = packed record
         case integer of
	    0: (v: array[0..5] of int32);
	    1: (xLo, yLo, zLo, xHi, yHi, zHi: int32);
         end;
 TVec6 = packed record
         case integer of
	    0: (v: array[0..5] of single);
	    1: (xLo, yLo, zLo, xHi, yHi, zHi: single);
         end;

  function prod(v:TVec3i): int64;
  function SetRGBA(r,g,b,a: byte): TRGBA;
  function pti(x,y,z: integer): TVec3i; //create integer vector
  procedure SortVec3i(var lo, hi: TVec3i);
  function  loadShaderPrefs(shaderName: string): TShaderPrefs;
  function ResourceDir (): string;
  function ScriptDir (): string;
  function ResourceFile (name: pchar; ofType: pchar): string;
  function ShaderDir (): string;
  function Vec6 (xLo, yLo, zLo, xHi, yHi, zHi: single): TVec6;


implementation

function Vec6 (xLo, yLo, zLo, xHi, yHi, zHi: single): TVec6;
begin
     result.xLo := xLo;
     result.yLo := yLo;
     result.zLo := zLo;
     result.xHi := xHi;
     result.yHi := yHi;
     result.zHi := zHi;

end;

{$IFDEF Darwin}

function ResourceDir (): string;
begin
	result := NSBundle.mainBundle.resourcePath.UTF8String;
end;

function ResourceURL (name: pchar; ofType: pchar): NSURL;
begin
	result := NSBundle.mainBundle.URLForResource_withExtension(NSSTR(name), NSSTR(ofType));
end;

function ResourceFile (name: pchar; ofType: pchar): string;
var
	url: NSURL;
begin
	url := ResourceURL(name, ofType);
	result := url.relativePath.UTF8String;
end;
{$ELSE}
function ResourceDir (): string;
begin
     result := extractfilepath(paramstr(0))+'Resources';
end;

function ResourceFile (name: pchar; ofType: pchar): string;
begin
     result := ResourceDir + pathdelim + name +'.'+ ofType;
end;
{$ENDIF}

function ScriptDir (): string;
begin
  result := ResourceDir + pathdelim + 'script';
end;

function ShaderDir (): string;
begin
	result := ResourceDir + pathdelim + 'shader';
end;



const
  kError = 666;
  kNote = 777;
  kBool = 0;
  kInt = 1;
  kFloat = 2;
  kSet = 3;


function StrToUniform(lS: string): TUniform;
var
  lV: string;
  lC: char;
  lLen,lP,lN: integer;
begin
  result.Name := '';
  result.Hint := '';
  result.Widget := kError;
  lLen := length(lS);
  //read values
  lV := '';
  lP := 1;
  lN := 0;
  while (lP <= lLen) do begin
    if lS[lP] = '/' then begin
       inc(lP);
      continue;
    end;
    if lS[lP] <> '|' then
      lV := lV + lS[lP];
    if (lS[lP] = '|') or (lP = lLen) then begin
        inc(lN);
        case lN of
          1: result.Name := lV;
          2: begin
              lC := upcase (lV[1]);
              case lC of
                'S' : result.Widget := kSet;
                'B' : result.Widget := kBool;
                'I' : result.Widget := kInt;
                'F' : result.Widget := kFloat;
                'N' : begin
                    result.Widget := kNote;
                    exit;
                  end;
                else
                  writeln('Unkown uniform type :'+lV);
                  exit;
              end;
            end;
          3: begin
            if (result.Widget = kBool) {or (result.Widget = kSet)} then begin
              result.bool := upcase (lV[1]) = 'T';
            end else
              result.min := strtofloatdef(lV,0);
            end;
          4: result.defaultv := strtofloatdef(lV,0);
          5: result.max := strtofloatdef(lV,0);
          6: result.Hint := lV;
        end;
        lV := '';
    end;
    inc(lP);
  end;
end;


function  loadShaderPrefs(shaderName: string): TShaderPrefs;
//load custom shader uniforms, between "//pref" and "//frag" tag
//pref
//ambient|float|0.0|1.0|2
//diffuse|float|0.0|0.3|2
//specular|float|0.0|0.25|2
//vert
//... rest of shader
const
  knone=0;
  kpref=1;
  kvert = 2;
  kfrag = 3;
var
  mode: integer;
  F : TextFile;
  S: string;
  U: TUniform;
begin
  result.nUniform := 0;
  if shaderName = '' then exit;
  if not fileexists(shaderName) then begin
     writeln('ShaderPrefs Unable to find '+  shaderName);
     exit;
 end;
 mode := knone;
 FileMode := fmOpenRead;
 AssignFile(F,shaderName);
 Reset(F);
 while (not Eof(F)) and (mode <> kvert) and (mode <> kfrag) do begin
   ReadLn(F, S);
   if S = '//pref' then
     mode := kpref
   else if S = '//frag' then
     mode := kfrag
   else if S = '//vert' then
     mode := kvert
   else if mode = kpref then begin
     //mode := kpref
      //writeln('>>>>>>>>>>'+S);
      U := StrToUniform(S);
     if (U.Widget = kFloat)then begin
        if (result.nUniform < kMaxUniform) then begin
          inc(result.nUniform);
          result.Uniform[result.nUniform] := U;
        end else
          writeln('Too many preferences');
      end ;
   end;
 end;//EOF
 CloseFile(F);
end;

procedure LoHi(var lo,hi: integer);
var
  s: integer;
begin
     if lo <= hi then exit;
     s := hi;
     hi := lo;
     lo := s;
end;

procedure SortVec3i(var lo, hi: TVec3i);
begin
     LoHi(lo.X, hi.X);
     LoHi(lo.Y, hi.Y);
     LoHi(lo.Z, hi.Z);
end;

function prod(v:TVec3i): int64;
begin
     result := v.x * v.y * v.z;
end;

function pti(x,y,z: integer): TVec3i; //create integer vector
begin
  result.X := x;
  result.Y := y;
  result.Z := z;
end; // pti()

function SetRGBA(r,g,b,a: byte): TRGBA;
begin
     result.r := r;
     result.g := g;
     result.b := b;
     result.a := a;
end;

end.

