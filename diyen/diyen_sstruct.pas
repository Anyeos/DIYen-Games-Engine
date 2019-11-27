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

*)
unit diyen_sstruct;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sfile, inifiles;

type

  // The structure type
  // It emulates the use of structures to save or load data from to files.
  TStruct = class(TComponent)
  public
    procedure Save( filename : string );
    function Load( filename : string ): Boolean;
  end;


  (*
    Configuraciones human readables.
    Esta clase te permite almacenar valores tipo de configuración
    en un archivo y luego volverlos a cargar.
    Los archivos pueden ser editados en texto plano y modificados
    sin problema.
  *)

  TReadableConfigKey = class
  Private
    FIdent: string;
    FValue: string;
  public
    constructor Create(const AIdent, AValue: string);
    property Ident: string read FIdent write FIdent;
    property Value: string read FValue write FValue;
  end;

  TReadableConfigKeyList = class(TList)
  private
    function GetItem(Index: integer): TReadableConfigKey;
    function KeyByName(const AName: string): TReadableConfigKey;
  public
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: integer]: TReadableConfigKey read GetItem; default;
  end;

  TReadableConfigSection = class
  private
    FName: string;
    FKeyList: TReadableConfigKeyList;
  public
    Function Empty : Boolean;
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property KeyList: TReadableConfigKeyList read FKeyList;
  end;


  TReadableConfigSectionList = class(TList)
    private
      function GetItem(Index: integer): TReadableConfigSection;
      function SectionByName(const AName: string): TReadableConfigSection;
    public
      destructor Destroy; override;
      procedure Clear;override;
      property Items[Index: integer]: TReadableConfigSection read GetItem; default;
  end;


  TReadableConfig = class(TObject)
    private
      FSectionList: TReadableConfigSectionList;
      procedure FillSectionList(AStrings: TStrings);
      function GetString(const Section, Name: string) : string;
      procedure SetString(const Section, Name: string; Value: string);
      function GetInteger(const Section, Name: string) : integer;
      procedure SetInteger(const Section, Name: string; Value: integer);
      function GetFloat(const Section, Name: string) : single;
      procedure SetFloat(const Section, Name: string; Value: single);
    public
      constructor Create;
      destructor Destroy; override;
      property Str[const Section, Name: string]: string read GetString write SetString;
      property Int[const Section, Name: string]: integer read GetInteger write SetInteger;
      property Float[const Section, Name: string]: single read GetFloat write SetFloat;
      procedure Save( filename : string );
      procedure Load( filename : string );
    end;


implementation

(*
  ======================
  TStruct implementation
  ======================
*)

// Save a "struct".
// In this case you must create a TStruct object
// then you can use it as structure containing
// data like in DIV
// Esta función te permite guardar una struct en
// un archivo como se hacía en DIV.
procedure TStruct.Save( filename : string );
var
  stream : TStream;
begin
  try
    stream := SSaveToFile(filename);
    stream.WriteComponent(self);
    stream.Free;
  except
  end;
end;

function TStruct.Load( filename : string ): Boolean;
var
  stream : TStream;
begin
  Result := true;
  try
    stream := SLoadFromFile(filename);
    stream.ReadComponent(self);
    stream.Free;
  except
    Result := false;
  end;
end;


(*
  ==============================
  TReadableConfig implementation
  ==============================
*)
constructor TReadableConfig.Create;
begin
  FSectionList := TReadableConfigSectionList.Create;
end;
destructor TReadableConfig.Destroy;
begin
  FSectionList.Free;

  inherited;
end;

function IsComment(const AString: string): boolean;
begin
  Result:=(Length(aString)>0) and (Copy(AString, 1, 1) = '#');
end;


constructor TReadableConfigKey.Create(const AIdent, AValue: string);
begin
  FIdent := AIdent;
  FValue := AValue;
end;

function TReadableConfigKeyList.GetItem(Index: integer): TReadableConfigKey;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TReadableConfigKey(inherited Items[Index]);
end;


function TReadableConfigKeyList.KeyByName(const AName: string): TReadableConfigKey;
var
  i: integer;
begin
  Result := nil;
  if (AName > '') and not IsComment(AName) then
      for i := 0 to Count-1 do
        if CompareText(Items[i].Ident, AName) = 0 then begin
          Result := Items[i];
          Break;
        end;
end;

procedure TReadableConfigKeyList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited Clear;
end;


destructor TReadableConfigKeyList.Destroy;
begin
  Clear;
  inherited Destroy;
end;


constructor TReadableConfigSection.Create(const AName: string);
begin
  FName := AName;
  FKeyList := TReadableConfigKeyList.Create;
end;

destructor TReadableConfigSection.Destroy;
begin
  FKeyList.Free;
end;


function TReadableConfigSection.Empty : Boolean;
Var
  I : Integer;
begin
  Result:=True;
  I:=0;
  While Result and (I<KeyList.Count)  do
    begin
    result:=IsComment(KeyList[i].Ident);
    Inc(i);
    end;
end;



function TReadableConfigSectionList.GetItem(Index: integer): TReadableConfigSection;
begin
  Result := nil;
  if (Index >= 0) and (Index < Count) then
    Result := TReadableConfigSection(inherited Items[Index]);
end;


function TReadableConfigSectionList.SectionByName(const AName: string): TReadableConfigSection;
var
  i: integer;
begin
  Result := nil;
  if (AName > '') and not IsComment(AName) then
      for i := 0 to Count-1 do
        if CompareText(Items[i].Name, AName) = 0 then
          begin
          Result := Items[i];
          Break;
          end;
end;

procedure TReadableConfigSectionList.Clear;
var
  i: integer;
begin
  for i := Count-1 downto 0 do
    Items[i].Free;
  inherited Clear;
end;

destructor TReadableConfigSectionList.Destroy;
begin
  Clear;
  inherited Destroy;
end;



function TReadableConfig.GetString(const Section, Name: string) : string;
var
  oSection: TReadableConfigSection;
  oKey: TReadableConfigKey;
  //J: integer;
begin
    Result := '';
    oSection := FSectionList.SectionByName(Section);
    if oSection <> nil then
    begin
      oKey := oSection.KeyList.KeyByName(Name);
      if oKey <> nil then
        Result:=oKey.Value;
    end;
end;

procedure TReadableConfig.SetString(const Section, Name: string; Value: string);
var
  oSection: TReadableConfigSection;
  oKey: TReadableConfigKey;
begin
    if (Section > '') and (Name > '') then
      begin
      // update or add key
      oSection := FSectionList.SectionByName(Section);
      if (oSection = nil) then
        begin
        oSection := TReadableConfigSection.Create(Section);
        FSectionList.Add(oSection);
        end;
      with oSection.KeyList do
        begin
        oKey := KeyByName(Name);
        if oKey <> nil then
          oKey.Value := Value
        else
          oSection.KeyList.Add(TReadableConfigKey.Create(Name, Value));
        end;
      end;
end;

function TReadableConfig.GetInteger(const Section, Name: string) : integer;
begin
  Result := StrToIntDef(GetString(Section, Name), 0);
end;

procedure TReadableConfig.SetInteger(const Section, Name: string; Value: integer);
begin
  SetString(Section, Name, IntToStr(Value));
end;

function TReadableConfig.GetFloat(const Section, Name: string) : single;
begin
  Result := StrToFloatDef(GetString(Section, Name), 0.0);
end;

procedure TReadableConfig.SetFloat(const Section, Name: string; Value: single);
begin
  SetString(Section, Name, FloatToStr(Value));
end;


procedure TReadableConfig.FillSectionList(AStrings: TStrings);
const
  Utf8Bom    = #$EF#$BB#$BF;        { Die einzelnen BOM Typen }

var
  i,j,sLen: integer;
  sLine, sIdent, sValue: string;
  oSection: TReadableConfigSection;

  procedure RemoveBackslashes;
  var
    i,l: integer;
    s: string;
    //bAppendNextLine, bAppended: boolean;
  begin
    AStrings.BeginUpdate;
    try
      For I:=AStrings.Count-2 downto 0 do
        begin
        S:=AStrings[i];
        L:=Length(S);
        If (I<AStrings.Count-1) and (L>0) and (S[L]='\') then
          begin
          S:=Copy(S,1,L-1)+AStrings[I+1];
          AStrings.Delete(I+1);
          AStrings[i]:=S;
          end;
        end;
    finally
      AStrings.EndUpdate;
    end;
  end;

Var
  addKey : Boolean;

begin
  oSection := nil;
  FSectionList.Clear;
  if (AStrings.Count > 0) then
    begin
    sLine:=AStrings[0];
    (*if (copy(sLine,1,Length(Utf8Bom)) = Utf8Bom) then
      begin
      FBOM := Utf8Bom;
      AStrings[0]:=copy(sLine,Length(Utf8Bom)+1,Length(SLine));
      end;*)
    end;
  for i := 0 to AStrings.Count-1 do begin
    sLine := Trim(AStrings[i]);
    sLen:=Length(sLine);
    if (sLen>0)  then
      begin
      if IsComment(sLine) and (oSection = nil) then
        begin
        // comment at the beginning of the ini file
        //if Not (ifoStripComments in Options) then
          begin
          oSection := TReadableConfigSection.Create(sLine);
          FSectionList.Add(oSection);
          end;
        continue;
        end;
      if (sLine[1]='[') and (sLine[sLen]= ']') then
        begin
        // regular section
        oSection := TReadableConfigSection.Create(Copy(sLine, 2, sLen - 2));
        FSectionList.Add(oSection);
        end
      else if oSection <> nil then
        begin
        if IsComment(sLine) then
          begin
          //AddKey:=Not (ifoStripComments in Options);
          AddKey := true;
          // comment within a section
          sIdent := sLine;
          sValue := '';
          end
        else
          begin
          // regular key
          j:=Pos(':', sLine);
          if j=0 then
           begin
           //AddKey:=Not (ifoStripInvalid in Options);
           AddKey:=true;
           sIdent:='';
           sValue:=sLine
           end
          else
           begin
           AddKey:=True;
           sIdent:=Trim(Copy(sLine, 1,  j - 1));
           sValue:=Trim(Copy(sLine, j + 1, sLen - j));
           end;
        end;
        if AddKey then
          oSection.KeyList.Add(TIniFileKey.Create(sIdent, sValue));
        end;
      end;
  end;
end;


procedure TReadableConfig.Save( filename : string );
var
  slLines: TStringList;
  i, j: integer;
  //D : String;

  Stream: TStream;
begin
  slLines := TStringList.Create;
  try
    for i := 0 to FSectionList.Count-1 do
      with FSectionList[i] do begin
        if IsComment(Name) then
          // comment
          slLines.Add(Name)
        else
          // regular section
          slLines.Add('[' + Name + ']');
        for j := 0 to KeyList.Count-1 do
          if IsComment(KeyList[j].Ident) then
            // comment
            slLines.Add(KeyList[j].Ident)
          else
            // regular key
            slLines.Add(KeyList[j].Ident + ':' + KeyList[j].Value);
        if (i < FSectionList.Count-1) and not IsComment(Name) then
          slLines.Add('');
      end;
    if slLines.Count > 0 then
      slLines.Strings[0] := slLines.Strings[0];

    Stream := SSaveToFile(filename);
    slLines.SaveToStream(Stream);

    FillSectionList(slLines);
  finally
    slLines.Free;
    Stream.Free;
  end;
end;

procedure TReadableConfig.Load( filename : string );
var
  slLines: TStringList;
  Stream: TStream;
begin
  slLines := TStringList.Create;
  try
    Stream := SLoadFromFile(filename);

    slLines.LoadFromStream(Stream);
    FillSectionList(slLines);
  finally
    slLines.Free;
    Stream.Free;
  end;
end;

end.
