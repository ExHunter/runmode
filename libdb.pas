{*****************************************}
{                                         }
{             libdb unit v1.0             }
{                                         }
{       Copyright (c) 2015 ExHunter       }
{          libdb stuff  by SyavX          }
{                                         }
{                                         }
{*****************************************}

unit libdb;

interface

uses
  constants;

// =====================================
//          C O N S T A N T S
// =====================================
{ Database plugins enumeration
  for the DB_Open() function   }
Const DB_Plugin_ODBC       = 1;
Const DB_Plugin_SQLite     = 2;
Const DB_Plugin_PostgreSQL = 3;

{ Database column types enumeration 
  for the DB_ColumnType() function  }
Const DB_Type_Double = 1;
Const DB_Type_Float  = 2;
Const DB_Type_Long   = 3;
Const DB_Type_String = 4;

// =====================================
//        D E C L A R A T I O N S
// =====================================
Procedure DB_Close(DatabaseID: Integer);
External 'DB_Close@libdb-0.2.dll cdecl';

Function DB_ColumnName(DatabaseID, Column: Integer): PChar;
External 'DB_ColumnName@libdb-0.2.dll cdecl';

Function DB_ColumnSize(DatabaseID, Column: Integer): Integer;
External 'DB_ColumnSize@libdb-0.2.dll cdecl';

Function DB_ColumnType(DatabaseID, Column: Integer): Integer;
External 'DB_ColumnType@libdb-0.2.dll cdecl';

Function DB_Columns(DatabaseID: Integer): Integer;
External 'DB_Columns@libdb-0.2.dll cdecl';

Function DB_Error(): PChar;
External 'DB_Error@libdb-0.2.dll cdecl';

Function DB_Query(DatabaseID: Integer; Query: PChar): Integer;
External 'DB_Query@libdb-0.2.dll cdecl';

Function DB_Update(DatabaseID: Integer; Query: PChar): Integer;
External 'DB_Update@libdb-0.2.dll cdecl';

Procedure DB_FinishQuery(DatabaseID: Integer);
External 'DB_FinishQuery@libdb-0.2.dll cdecl';

Function DB_FirstRow(DatabaseID: Integer): Integer;
External 'DB_FirstRow@libdb-0.2.dll cdecl';

Function DB_GetDouble(DatabaseID, Column: Integer): Double;
External 'DB_GetDouble@libdb-0.2.dll cdecl';

Function DB_GetFloat(DatabaseID, Column: Integer): Single;
External 'DB_GetFloat@libdb-0.2.dll cdecl';

Function DB_GetLong(DatabaseID, Column: Integer): LongInt;
External 'DB_GetLong@libdb-0.2.dll cdecl';

Function DB_GetString(DatabaseID, Column: Integer): PChar;
External 'DB_GetString@libdb-0.2.dll cdecl';

Function DB_IsDatabase(DatabaseID: Integer): Integer;
External 'DB_IsDatabase@libdb-0.2.dll cdecl';

Function DB_NextRow(DatabaseID: Integer): Integer;
External 'DB_NextRow@libdb-0.2.dll cdecl';

Function DB_Open(DatabaseID: Integer; DatabaseName, User, Password: PChar; Plugin: Integer): Integer;
External 'DB_Open@libdb-0.2.dll cdecl';

Function DB_ExamineDrivers(): Integer;
External 'DB_ExamineDrivers@libdb-0.2.dll cdecl';

Function DB_NextDriver(): Integer;
External 'DB_NextDriver@libdb-0.2.dll cdecl';

Function DB_DriverDescription(): PChar;
External 'DB_DriverDescription@libdb-0.2.dll cdecl';

Function DB_DriverName(): PChar;
External 'DB_DriverName@libdb-0.2.dll cdecl';

Function DB_GetVersion(): Integer;
External 'DB_GetVersion@libdb-0.2.dll cdecl';

procedure DB_Establish_Connection();
procedure DB_Ping_Server();

function DB_Escape_String(input: string): string;
procedure DB_PerformQuery(used_loc: string; Query: PChar);
procedure DB_PerformConnectedQuery(used_loc: string; Query: PChar);

function DB_Query_Replace_Val1(Query, Val1: string): string;
function DB_Query_Replace_Val2(Query, Val1, Val2: string): string;
function DB_Query_Replace_Val3(Query, Val1, Val2, Val3: string): string;
function DB_Query_Replace_Val4(Query, Val1, Val2, Val3, Val4: string): string;
function DB_Query_Replace_Val5(Query, Val1, Val2, Val3, Val4, Val5: string): string;
function DB_Query_Replace_Val6(Query, Val1, Val2, Val3, Val4, Val5, Val6: string): string;

var
  DB_CONNECTED: Boolean;

implementation

procedure DB_Establish_Connection();
begin
  if DB_IsDatabase(DB_ID) = 0 then
  begin
    if DB_Open(DB_ID, DB_CONNECTION_STRING, DB_USER, DB_PASS, DB_Plugin_ODBC) <> 0 then
    begin
      DB_CONNECTED := true;
      WriteLn('[DB] Successfully connected to the Database!');
    end else
    begin
      WriteLn('[DB_ERROR (DB_Establish_Connection)] ' + DB_Error());
      DB_CONNECTED := false;
    end;
  end else
  begin
    WriteLn('[DB] Already connected to the Database!');
    DB_CONNECTED := true;
    WriteLn('[DB] Refreshed DB_CONNECTED variable!');
  end;
end;

procedure DB_Ping_Server();
begin
  WriteLn('[DB] Pinging MySQL-Server to keep connection alive...');
  if DB_CONNECTED then
  begin
    if DB_Update(DB_ID, SQL_PING_SERVER) = 0 then
      WriteLn('[DB] Error in DB_Ping_Server: ' + DB_Error());
  end else
  begin
    WriteLn('[DB] Error in DB_Ping_Server: Not connected to the Database!');
    WriteLn('[DB] Trying to establish a new connection...');
    DB_Establish_Connection;
  end;
end;


function DB_Escape_String(Input: string): string;
begin
  Result := ReplaceRegExpr('''', Input, '''''', FALSE);
  Result := ReplaceRegExpr('\',  Result, '\\',  FALSE);
end;

procedure DB_PerformQuery(used_loc: string; Query: PChar);
begin
  if DB_Update(DB_ID, Query) = 0 then
    WriteLn('[DB] Error in ' + used_loc + ': ' + DB_Error());
end;

procedure DB_PerformConnectedQuery(used_loc: string; Query: PChar);
begin
  if DB_CONNECTED then
  begin
    if DB_Update(DB_ID, Query) = 0 then
      WriteLn('[DB] Error in ' + used_loc + ': ' + DB_Error());
  end else
    WriteLn('[DB] Error in ' + used_loc + ': Not connected to the Database!');
end;



function DB_Query_Replace_Val1(Query, Val1: string): string;
begin
  Result := ReplaceRegExpr('VAL1', Query, Val1, FALSE);
end;

function DB_Query_Replace_Val2(Query, Val1, Val2: string): string;
begin
  Result := ReplaceRegExpr('VAL1', Query,  Val1, FALSE);
  Result := ReplaceRegExpr('VAL2', Result, Val2, FALSE);
end;

function DB_Query_Replace_Val3(Query, Val1, Val2, Val3: string): string;
begin
  Result := ReplaceRegExpr('VAL1', Query,  Val1, FALSE);
  Result := ReplaceRegExpr('VAL2', Result, Val2, FALSE);
  Result := ReplaceRegExpr('VAL3', Result, Val3, FALSE);
end;

function DB_Query_Replace_Val4(Query, Val1, Val2, Val3, Val4: string): string;
begin
  Result := ReplaceRegExpr('VAL1', Query,  Val1, FALSE);
  Result := ReplaceRegExpr('VAL2', Result, Val2, FALSE);
  Result := ReplaceRegExpr('VAL3', Result, Val3, FALSE);
  Result := ReplaceRegExpr('VAL4', Result, Val4, FALSE);
end;

function DB_Query_Replace_Val5(Query, Val1, Val2, Val3, Val4, Val5: string): string;
begin
  Result := ReplaceRegExpr('VAL1', Query,  Val1, FALSE);
  Result := ReplaceRegExpr('VAL2', Result, Val2, FALSE);
  Result := ReplaceRegExpr('VAL3', Result, Val3, FALSE);
  Result := ReplaceRegExpr('VAL4', Result, Val4, FALSE);
  Result := ReplaceRegExpr('VAL5', Result, Val5, FALSE);
end;

function DB_Query_Replace_Val6(Query, Val1, Val2, Val3, Val4, Val5, Val6: string): string;
begin
  Result := ReplaceRegExpr('VAL1', Query,  Val1, FALSE);
  Result := ReplaceRegExpr('VAL2', Result, Val2, FALSE);
  Result := ReplaceRegExpr('VAL3', Result, Val3, FALSE);
  Result := ReplaceRegExpr('VAL4', Result, Val4, FALSE);
  Result := ReplaceRegExpr('VAL5', Result, Val5, FALSE);
  Result := ReplaceRegExpr('VAL6', Result, Val6, FALSE);
end;

end.