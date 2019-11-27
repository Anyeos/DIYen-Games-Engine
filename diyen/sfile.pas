(*
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.


  StreamFile
  Two little functions to work with files in a cross-platform fashion
  (windows, linux, and specially android).
  The advantage of this in android is that you will aquire the files
  directly from the "asset" directory specifing relative paths.
  And you too can save a file if you only specify relative paths it will
  be saved in your app private data directory automatically. And you can
  load that next time too.

  Copyright (C) 2013 Anyeos

  How to use:
  With this functions you only need to do something like:

  uses sfile;
  ...
    var
    Stream: TStream;
    begin
      try
        Stream := SLoadFromFile('myfile.ext');
        myStringList.LoadFromStream(Stream);
        Stream.Free;
      except
      end;
    end;

  For saving is likewise:
  Stream := SSaveToFile('myfile.ext');
  myStringList.SaveToStream(Stream);
  Stream.Free;

  Using this functions will warantee that you will always load and
  save your files not matter where you are running (android, windows, linux).
  It too will load automatically from the assets inside the apk without you
  need to doing nothing.

  It uses SDL2 to work and optionally need SDL_Log.
  If you don't want SDL_Log simply comment the lines with SDL_Log.
**)
unit sfile;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, SDL2;

function SLoadFromFile(const FileName : string) : TStream;
function SSaveToFile(const FileName: string) : TStream;

type
TSFileStream = class(TStream)
private
  FFileName : PChar;
  FHandle: PSDL_RWops;
protected
  //procedure SetSize(NewSize: Longint); override;
  //procedure SetSize(const NewSize: Int64); override;
  function RWread(out Buffer; Count: Longint): Longint;
  function RWwrite(const Buffer; Count: Longint): Longint;
public
  constructor Create(const AFileName: string; Mode: Word);
  destructor Destroy; override;
  property FileName : PChar Read FFilename;
  function Read(var Buffer; Count: Longint): Longint; override;
  function Write(const Buffer; Count: Longint): Longint; override;
  function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  property Handle: PSDL_RWops read FHandle;
end;


implementation

constructor TSFileStream.Create(const AFileName: string; Mode: Word);
  begin
    FFileName:=PChar(AFileName);

    If (Mode and fmCreate) = fmCreate then
      FHandle:=SDL_RWFromFile(FFileName, 'wb')
    else
      FHAndle:=SDL_RWFromFile(FFileName, 'rb');

    If (FHandle = nil) then
      If Mode=fmCreate then
        begin
        {$ifdef DEBUG}
        SDL_Log(PChar('Error creating the file: "'+FFileName+'". '+SDL_GetError()));
        {$endif}
        raise EFCreateError.CreateFmt(SDL_GetError(),[AFileName]);
        end
      else
      begin
        {$ifdef DEBUG}
        //SDL_Log(PChar('Error opening the file: "'+FFileName+'". '+SDL_GetError()));
        {$endif}
        raise EFOpenError.CreateFmt(SDL_GetError(),[AFilename]);
      end;
  end;

function TSFileStream.RWread(out Buffer; Count: Longint): Longint;
begin
  repeat
  Result := SDL_RWread(FHandle, pchar(@Buffer), 1, Count);
  until (Result <> -1);
end;

function TSFileStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := RWread(Buffer, Count);
  if Result = -1 then Result := 0;
end;

function TSFileStream.RWwrite(const Buffer; Count: Longint): Longint;
begin
  repeat
  Result := SDL_RWwrite(FHandle, pchar(@Buffer), 1, Count);
  until (Result <> -1);
end;

function TSFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := RWwrite(Buffer, Count);
  if Result = -1 then Result := 0;
end;

function TSFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := SDL_RWseek(FHandle,Offset,ord(Origin));
end;

destructor TSFileStream.Destroy;
begin
  if (FHandle <> nil) then SDL_RWclose(FHandle);
end;

(*{$IFNDEF ANDROID}

function SLoadFromFile(const FileName : string) : TStream;
Var TheStream : TFileStream;
begin
  TheStream:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  SLoadFromFile := TheStream;
end;

function SSaveToFile(const FileName: string) : TStream;
Var TheStream : TFileStream;
begin
  TheStream:=TFileStream.Create(FileName,fmCreate);
  SSaveToFile := TheStream;
end;

{$ELSE}*)
function SLoadFromFile(const FileName : string) : TStream;
Var TheStream : TSFileStream;
begin
  TheStream:=TSFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  SLoadFromFile := TheStream;
end;

function SSaveToFile(const FileName: string) : TStream;
Var TheStream : TSFileStream;
//Var TheStream : TFileStream;
begin
  TheStream:=TSFileStream.Create(FileName,fmCreate);
  SSaveToFile := TheStream;
end;
//{$ENDIF}

begin
end.

