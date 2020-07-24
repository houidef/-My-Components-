{************************************************************ Version Original V0.03 build 1 }  
unit Flux;

interface

uses
  SysUtils, Classes,dialogs;

type
  TFlux = class(TFileStream)
    function MyRead():string;//(?:TFluxCdn; ?:PShortString);
    procedure MyWrite(buffer : String);//(?:TFluxCdn; ?:ShortString);
  end;

implementation

//004B696C
function TFlux.MyRead():string;
var
  count : byte;
begin
  Read(count,1);
 SetLength(result,count);
 Read(result[1],count);
end;

//004B6A80
procedure TFlux.MyWrite(buffer : String);
var
  count:Byte;
begin
  count:=Length(buffer);
  WriteBuffer(count,1);
  WriteBuffer(buffer[1],count);
end;

end.