{*****************************************}
{                                         }
{          RunModeSC3 unit  v1.0          }
{                                         }
{       Copyright (c) 2015 ExHunter       }
{                                         }
{     Original idea + script by Vince     }
{                                         }
{*****************************************}

unit RunModeSC3;

interface

uses
  constants, libdb;

type
  TVectorAdvanced = record
    X, Y: Single;
    Checked: Boolean;
    Distance: Single;
    end;

  TMapVariables = record
    Loaded: Boolean;
    TimeRunningLeft: Integer;
    MapID: Integer;
    RunnerStartSpawn: TVector;
    CheckPoints: Array of TVectorAdvanced;
    AmountOfCheckpoints: Byte;
    AmountOfLaps: Byte;
    CurrentLap: Byte;
    NextMap: string;
    end;

  TBestRun = record
    CheckPoint: Array of Single;
    end;

  TRunnerProperties = record
    ID: Byte;
    PPlayer: TActivePlayer;
    StartTime: TDateTime;
    Laps: Byte;
    end;

  TGameVariables = record
    Countdown, Seconds: Integer;
    Active: Boolean;
    Map: TMapVariables;
    BestRunLap: Array of TBestRun;
    BestRunLoaded: Boolean;
    CurrentRunLap: Array of TBestRun;
    Runner: TRunnerProperties;
    end;

  TReplay = record
    KeyUp, KeyLeft, KeyRight, KeyJetpack, KeyGrenade, KeyChangeWeap, KeyThrow, KeyCrouch, KeyProne: Boolean;
    AimX, AimY: SmallInt;
    PosX, PosY: Single;
    end;

  TPointers = record
    Clock_Normal, Clock_Load_Replay, Clock_Wait_Time: TOnClockTickEvent;
    end;

  TQueue = record
    Tail: Byte;
    Members: Array[1..QUEUE_MAX_PLAYERS] of Byte;
    end;

var
  RM: TGameVariables;
  HighID: Byte;
  ReplayValues: Array of TReplay;
  ReplayBot: TActivePlayer;
  BotActive: Boolean;
  CurrentLoop: Integer;
  Pointers: TPointers;
  WHITESPACES: Array of string;
  Queue: TQueue;

implementation

procedure WriteLnAndConsole(p: TActivePlayer; Text: string; Color: Cardinal);
begin
  WriteLn(Text);
  if p = NIL then
    Players.WriteConsole(Text, Color)
  else
    p.WriteConsole(Text, Color);
end;

procedure DecideIfWriteLnOrConsole(p: TActivePlayer; Text: string; Color: Cardinal);
begin
  if p = NIL then
    WriteLn(Text)
  else
    p.WriteConsole(Text, Color);
end;

function IsInEditorMode(p: TActivePlayer): Boolean;
begin
  Result := p.Team = TEAM_EDITOR;
end;

function IsQueueEmpty(): Boolean;
begin
  Result := Queue.Tail = 0;
end;

function QueuePosition(MemberID: Byte): Byte;
var
  I: Byte;
begin
  Result := 0;
  if not IsQueueEmpty() then
    for I := 1 to Queue.Tail do
      if Queue.Members[I] = MemberID then
      begin
        Result := I;
        Break;
      end;
end;

function RemoveFromQueue(MemberID: Byte): Boolean;
var
  I: Byte;
  QueuePosResult: Byte;
begin
  if IsQueueEmpty() then
    Result := False
  else
  begin
    QueuePosResult := QueuePosition(MemberID);
    if QueuePosResult = 0 then
      Result := False
    else
    begin
      for I := QueuePosResult to Queue.Tail do
        if I < QUEUE_MAX_PLAYERS then
          Queue.Members[I] := Queue.Members[I + 1]
        else
          Queue.Members[I] := 0;
      Queue.Tail := Queue.Tail - 1;
      Result := True;
    end;
  end;
end;

function DeQueue(): Byte;
var
  I: Byte;
begin
  if IsQueueEmpty() then
    Result := 0
  else
  begin
    Result := Queue.Members[1];
    if Queue.Tail > 1 then
    begin
      for I := 1 to Queue.Tail - 1 do
        Queue.Members[I] := Queue.Members[I + 1];
    end else
      Queue.Members[1] := 0;
    Queue.Tail := Queue.Tail - 1;
  end;
end;

function EnQueue(NewMember: Byte): Boolean;
begin
  if Queue.Tail < QUEUE_MAX_PLAYERS then
  begin
    Queue.Tail := Queue.Tail + 1;
    Queue.Members[Queue.Tail] := NewMember;
  end else
    Result := False;
end;

function ReQueue(): Boolean;
begin
  Result := EnQueue(DeQueue());
end;

function Explode_ReplayData(runID: Integer): Array of TReplay;
var
  Length: Integer;
begin
  try
    if DB_Open(DB_ID_REPLAYS, DB_CON_STRING_REPLAY, '', '', DB_Plugin_ODBC) <> 0 then
    begin
      if DB_Query(DB_ID_REPLAYS, DB_Query_Replace_Val2(SQL_GET_REPLAY, Game.CurrentMap, IntToStr(runID))) <> 0 then
      begin
        Length := 0;
        SetArrayLength(Result, 0);
        while DB_NextRow(DB_ID_REPLAYS) <> 0 do
        begin
          SetArrayLength(Result, Length + 1);
          Result[Length].KeyUp         := DB_GetLong(DB_ID_REPLAYS, 0) = 1; // `KeyUp`
          Result[Length].KeyLeft       := DB_GetLong(DB_ID_REPLAYS, 1) = 1; // `KeyLeft`
          Result[Length].KeyRight      := DB_GetLong(DB_ID_REPLAYS, 2) = 1; // `KeyRight`
          Result[Length].KeyJetpack    := DB_GetLong(DB_ID_REPLAYS, 3) = 1; // `KeyJetpack`
          Result[Length].KeyGrenade    := DB_GetLong(DB_ID_REPLAYS, 4) = 1; // `KeyGrenade`
          Result[Length].KeyChangeWeap := DB_GetLong(DB_ID_REPLAYS, 5) = 1; // `KeyChangeWeap`
          Result[Length].KeyThrow      := DB_GetLong(DB_ID_REPLAYS, 6) = 1; // `KeyThrow`
          Result[Length].KeyCrouch     := DB_GetLong(DB_ID_REPLAYS, 7) = 1; // `KeyCrouch`
          Result[Length].KeyProne      := DB_GetLong(DB_ID_REPLAYS, 8) = 1; // `KeyProne`
          Result[Length].AimX          := DB_GetLong(DB_ID_REPLAYS, 9);     // `AimX`
          Result[Length].AimY          := DB_GetLong(DB_ID_REPLAYS, 10);    // `AimY`
          Result[Length].PosX          := DB_GetFloat(DB_ID_REPLAYS, 11);   // `PosX`
          Result[Length].PosY          := DB_GetFloat(DB_ID_REPLAYS, 12);   // `PosY`
          Length := Length + 1;
        end;
        DB_FinishQuery(DB_ID_REPLAYS);
        DB_Close(DB_ID_REPLAYS);
      end else
      begin
        SetArrayLength(Result, 0);
        WriteLn('[RM] Error in Explode_ReplayData: ' + DB_Error());
      end;
    end else
    begin
      SetArrayLength(Result, 0);
      WriteLn('[RM] Could not open replay Database!');
      WriteLn('[RM] Error in Explode_ReplayData: ' + DB_Error());
    end;
  except
    SetArrayLength(Result, 0);
    DB_FinishQuery(DB_ID_REPLAYS);
    DB_Close(DB_ID_REPLAYS);
    WriteLn('[RM] Something failed in Explode_ReplayData! Closed replay Database!');
  end;

end;

function GetAllMedals(PlayerID: Integer; var GoldMedals, SilverMedals, BronzeMedals: Integer): Boolean;
begin
  GoldMedals := 0;
  SilverMedals := 0;
  BronzeMedals := 0;
  Result := False;
  if DB_CONNECTED then
  begin
    // PlayerID can be 0, for rankings who yet have no medals
    if PlayerID = 0 then
    begin
      Result := True;
      Exit;
    end;
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_PLR_MEDALS, IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      GoldMedals   := DB_GetLong(DB_ID, 0); // `gold`
      SilverMedals := DB_GetLong(DB_ID, 1); // `silver`
      BronzeMedals := DB_GetLong(DB_ID, 2); // `bronze`
      Result       := True;
    end;
  end else
    WriteLn('[RM] Error in ExchangeMedal: Database is not connected!');
end;

procedure ExchangeMedal(MedalType: Byte; PlayerWhoGets, PlayerWhoLoses: Integer);
var
  MedalCountGoldWinner, MedalCountSilverWinner, MedalCountBronzeWinner: Integer;
  MedalCountGoldLoser, MedalCountSilverLoser, MedalCountBronzeLoser: Integer;
begin
  if DB_CONNECTED then
  begin
    if GetAllMedals(PlayerWhoGets, MedalCountGoldWinner, MedalCountSilverWinner, MedalCountBronzeWinner) then
      if GetAllMedals(PlayerWhoLoses, MedalCountGoldLoser, MedalCountSilverLoser, MedalCountBronzeLoser) then
      begin
        case MedalType of
          MEDAL_GOLD:
          begin
            if PlayerWhoGets > 0 then
              DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_GOLDS, IntToStr(MedalCountGoldWinner + 1), IntToStr(PlayerWhoGets)));
            if (PlayerWhoLoses > 0) and (MedalCountGoldLoser > 0) then
              DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_GOLDS, IntToStr(MedalCountGoldLoser - 1), IntToStr(PlayerWhoLoses)));
          end;
          MEDAL_SILVER:
          begin
            if PlayerWhoGets > 0 then
              DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_SILVERS, IntToStr(MedalCountSilverWinner + 1), IntToStr(PlayerWhoGets)));
            if (PlayerWhoLoses > 0) and (MedalCountSilverLoser > 0) then
              DB_PerformQuery(DB_ID,'ExchangeMedal',  DB_Query_Replace_Val2(SQL_UPDATE_SILVERS, IntToStr(MedalCountSilverLoser - 1), IntToStr(PlayerWhoLoses)));
          end;
          MEDAL_BRONZE:
          begin
            if PlayerWhoGets > 0 then
              DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_BRONZES, IntToStr(MedalCountBronzeWinner + 1), IntToStr(PlayerWhoGets)));
            if (PlayerWhoLoses > 0) and (MedalCountBronzeLoser > 0) then
              DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_BRONZES, IntToStr(MedalCountBronzeLoser - 1), IntToStr(PlayerWhoLoses)));
          end;
        end;
      end;
  end else
    WriteLn('[RM] Error in ExchangeMedal: Database is not connected!');
end;

procedure NewBronzeMedal(NewPlayer, OldBronze: Integer);
begin
  if NewPlayer = OldBronze then
    Exit;
  if (NewPlayer > 0) or (OldBronze > 0) then
    ExchangeMedal(MEDAL_BRONZE, NewPlayer, OldBronze);
end;

procedure NewSilverMedal(NewPlayer, OldSilver, OldBronze: Integer);
begin
  if NewPlayer = OldSilver then
    Exit;
  if (NewPlayer > 0) or (OldSilver > 0) then
  begin
    ExchangeMedal(MEDAL_SILVER, NewPlayer, OldSilver);
    if NewPlayer = OldBronze then
    begin
      OldBronze := 0;
      ExchangeMedal(MEDAL_BRONZE, OldBronze, NewPlayer);
    end;
    NewBronzeMedal(OldSilver, OldBronze);
  end;
end;

procedure NewGoldMedal(NewPlayer, OldGold, OldSilver, OldBronze: Integer);
begin
  if NewPlayer = OldGold then
    Exit;
  if (NewPlayer > 0) or (OldGold > 0) then
  begin
    ExchangeMedal(MEDAL_GOLD, NewPlayer, OldGold);
    if NewPlayer = OldSilver then
    begin
      OldSilver := 0;
      ExchangeMedal(MEDAL_SILVER, OldSilver, NewPlayer);
    end;
    NewSilverMedal(OldGold, OldSilver, OldBronze);
  end;
end;

function GetPlayerRank(PlayerID, mapID: Integer): Integer;
begin
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, SQL_GET_RANK_1_OF_2) <> 0) and
       (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_RANK_2_OF_2, IntToStr(mapID),
        IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      Result := DB_GetLong(DB_ID, 0); // `rank`
      DB_FinishQuery(DB_ID);
    end else
    begin
      Result := 0;
      WriteLn('[RM] Error in GetPlayerRank: ' + DB_Error());
      DB_FinishQuery(DB_ID);
    end;
  end else
  begin
    Result := 0;
    WriteLn('[RM] Error in GetPlayerRank: Database is not connected!');
  end;
end;

function GetBestRunIDOnMap(MapID: Integer): Integer;
begin
  Result := 0;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_TOP_X, IntToStr(MapID), '1')) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      Result := DB_GetLong(DB_ID, 0); // `rm_mapstats`.`mapID`
      DB_FinishQuery(DB_ID);
    end else
    begin
      WriteLn('[RM] The Map with the ID ' + IntToStr(MapID) + ' was not found in the Database!');
      WriteLn('[RM] Error in GetBestRunIDOnMap: ' + DB_Error());
      DB_FinishQuery(DB_ID);
    end;
  end else
    WriteLn('[RM] Could not load the GetBestRunIDOnMap! Database is not connected!');
end;

procedure LoadBestRun(RunID: Integer);
var
  I: Byte;
begin
  if DB_Open(DB_ID_REPLAYS, DB_CON_STRING_REPLAY, '', '', DB_Plugin_ODBC) <> 0 then
  begin
    if DB_Query(DB_ID_REPLAYS, DB_Query_Replace_Val2(SQL_GET_BESTRUN, Game.CurrentMap, IntToStr(RunID))) <> 0 then
    begin
      SetArrayLength(RM.BestRunLap, RM.Map.AmountOfLaps);
      for I := 0 to RM.Map.AmountOfLaps - 1 do
        SetArrayLength(RM.BestRunLap[I].CheckPoint, RM.Map.AmountOfCheckpoints);
      try
        I := 0;
        while (DB_NextRow(DB_ID_REPLAYS) <> 0) do
        begin
          // `runID` = 0, `Lap` = 1, `CheckPoint` = 2, `Time` = 3
          RM.BestRunLap[DB_GetLong(DB_ID_REPLAYS, 1) - 1].CheckPoint[DB_GetLong(DB_ID_REPLAYS, 2) - 1] := StrToDateTime(DB_GetString(DB_ID_REPLAYS, 3));
          I := I + 1;
        end;
        if I = RM.Map.AmountOfLaps * RM.Map.AmountOfCheckpoints then
        begin
          RM.BestRunLoaded := True;
          WriteLn('[RM] Successfully loaded BestRun.');
        end else
          WriteLn('[RM] Amount of loaded BestRun data incorrect. Aborted.');
      except
        WriteLn('[RM] Some error occured while loading BestRun! Aborted.');
      end;
    end else
      WriteLn('[RM] This map has no best run yet!');
  end else
    WriteLn('[RM] The Database for Replays could not been open!');
  DB_FinishQuery(DB_ID_REPLAYS);
  DB_Close(DB_ID_REPLAYS);
end;

function Save_RunData(HWID: string; RunnerTime: TDateTime): Integer;
var
  ExistingRunID: Integer;
  PlayerID: Integer;
  DataBaseTime: TDateTime;
  GoldPlayer, SilverPlayer, BronzePlayer: Integer;
  PlayerNewRank: Integer;
begin
  Result := 0;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_PLAYER_ID, HWID)) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      PlayerID := DB_GetLong(DB_ID, 0); // `ID`
      DB_FinishQuery(DB_ID);

      // Search for other players medals before they get overwritten
      if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_TOP_X, IntToStr(RM.Map.MapID), '3')) <> 0) then
      begin
        if DB_NextRow(DB_ID) <> 0 then
        begin
          GoldPlayer := DB_GetLong(DB_ID, 1); // `rm_mapstats`.`playerID`
          if DB_NextRow(DB_ID) <> 0 then
          begin
            SilverPlayer := DB_GetLong(DB_ID, 1); // `rm_mapstats`.`playerID`
            if DB_NextRow(DB_ID) <> 0 then
              BronzePlayer := DB_GetLong(DB_ID, 1) // `rm_mapstats`.`playerID`
            else
              BronzePlayer := 0;
          end else
          begin
            SilverPlayer := 0;
            BronzePlayer := 0;
          end;
        end else
        begin
          GoldPlayer   := 0;
          SilverPlayer := 0;
          BronzePlayer := 0;
        end;
      end else
      begin
        GoldPlayer   := 0;
        SilverPlayer := 0;
        BronzePlayer := 0;
      end;
      DB_FinishQuery(DB_ID);

      // Insert running data now
      if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_RUN, IntToStr(PlayerID),
          IntToStr(RM.Map.MapID))) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
      begin
        // result here 0 too, if new run is worse than old
        // run, so that replay does not get overwritten
        WriteLn('[RM] The runner has a saved run here... Checking if new run is better...');
        ExistingRunID := DB_GetLong(DB_ID, 0);                  // `ID`
        DataBaseTime  := StrToDateTime(DB_GetString(DB_ID, 1)); // `runtime`
        DB_FinishQuery(DB_ID);
        if RunnerTime < DataBaseTime then
        begin
          WriteLn('[RM] The new run is better.. Updating his run...');
          DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val3(SQL_UPDATE_RUN,
            FormatDateTime('hh:nn:ss.zzz', RunnerTime), IntToStr(DB_SERVER_ID),
            IntToStr(ExistingRunID)));
          Result := ExistingRunID;
        end else
          WriteLn('[RM] The new run is worse.. Doing nothing.');
      end else
      begin
        WriteLn('[RM] The runner did not have a time yet! Adding a new one...');
        DB_FinishQuery(DB_ID);
        DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val4(SQL_ADD_RUN,
          IntToStr(RM.Map.MapID), IntToStr(PlayerID), IntToStr(DB_SERVER_ID),
          FormatDateTime('hh:nn:ss.zzz', RunnerTime)));
        DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val1(SQL_INC_RECORDNUM, IntToStr(RM.Map.MapID)));
        if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_RUN, IntToStr(PlayerID),
            IntToStr(RM.Map.MapID))) <> 0) and
           (DB_NextRow(DB_ID) <> 0) then
        begin
          Result := DB_GetLong(DB_ID, 0); // `ID`
          DB_FinishQuery(DB_ID);
        end;
      end;

      DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val1(SQL_INC_RUNSNUM, IntToStr(RM.Map.MapID)));
      DB_FinishQuery(DB_ID);

      // Now check for any difference in medals rankings
      if Result > 0 then // Result is over 0 if something changed
      begin
        PlayerNewRank := GetPlayerRank(PlayerID, RM.Map.MapID);
        case PlayerNewRank of
          MEDAL_GOLD:
          begin
          if PlayerID <> GoldPlayer then
            NewGoldMedal(PlayerID, GoldPlayer, SilverPlayer, BronzePlayer);
          end;
          MEDAL_SILVER:
          begin
            if PlayerID <> SilverPlayer then
              NewSilverMedal(PlayerID, SilverPlayer, BronzePlayer);
          end;
          MEDAL_BRONZE:
          begin
            if PlayerID <> BronzePlayer then
              NewBronzeMedal(PlayerID, BronzePlayer);
          end;
        end;
      end;
    end else
    begin
      Result := 0;
      WriteLn('[RM] Player with HWID ' + HWID + ' was not found in Database!');
      WriteLn('[RM] Error in Save_RunData: ' + DB_Error());
      DB_FinishQuery(DB_ID);
      Exit;
    end;
  end else
  begin
    Result := 0;
    WriteLn('[RM] Could not save RunData! Database is not connected!');
    Exit;
  end;
end;

// Rather complex here.. Too much input to do this via CONSTANTS and replaces.
function Save_ReplayData(runID: Integer; ReplayData: Array of TReplay): Boolean;
var
  I, J: Integer;
  runIDString: string;
  QueryString: string;
begin
  Result := True;

  if DB_Open(DB_ID_REPLAYS, DB_CON_STRING_REPLAY, '', '', DB_Plugin_ODBC) <> 0 then
  begin
    WriteLn('[RM] Deleting replay if exists...');
    runIDString := IntToStr(runID);
    DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', DB_Query_Replace_Val2(SQL_DELETE_REPLAY, Game.CurrentMap, runIDString));
    WriteLn('[RM] Inserting new replay data...');
    I := 0;
    QueryString := 'INSERT INTO `' + Game.CurrentMap +
    '` (`replayOrder`, `runID`, `KeyUp`, `KeyLeft`, `KeyRight`, `KeyJetpack`, `KeyGrenade`,' +
    ' `KeyChangeWeap`, `KeyThrow`, `KeyCrouch`, `KeyProne`, `AimX`, `AimY`, `PosX`, `PosY`) VALUES' + FILE_NEWLINE;
    QueryString := QueryString + FILE_NEWLINE + '(' + IntToStr(I) + ', ' + runIDString + ', ' +
                                                iif(ReplayData[I].KeyUp, '1', '0') + ', ' +
                                                iif(ReplayData[I].KeyLeft, '1', '0') + ', ' +
                                                iif(ReplayData[I].KeyRight, '1', '0') + ', ' +
                                                iif(ReplayData[I].KeyJetpack, '1', '0') + ', ' +
                                                iif(ReplayData[I].KeyGrenade, '1', '0') + ', ' +
                                                iif(ReplayData[I].KeyChangeWeap, '1', '0') + ', ' +
                                                iif(ReplayData[I].KeyThrow, '1', '0') + ', ' +
                                                iif(ReplayData[I].KeyCrouch, '1', '0') + ', ' +
                                                iif(ReplayData[I].KeyProne, '1', '0') + ', ' +
                                                IntToStr(ReplayData[I].AimX) + ', ' +
                                                IntToStr(ReplayData[I].AimY) + ', ' +
                                                FloatToStr(ReplayData[I].PosX) + ', ' +
                                                FloatToStr(ReplayData[I].PosY) + ')';
    for I := 1 to GetArrayLength(ReplayData) - 1 do
      QueryString := QueryString + ', ' + FILE_NEWLINE + '(' + IntToStr(I) + ', ' + runIDString + ', ' +
                                                         iif(ReplayData[I].KeyUp, '1', '0') + ', ' +
                                                         iif(ReplayData[I].KeyLeft, '1', '0') + ', ' +
                                                         iif(ReplayData[I].KeyRight, '1', '0') + ', ' +
                                                         iif(ReplayData[I].KeyJetpack, '1', '0') + ', ' +
                                                         iif(ReplayData[I].KeyGrenade, '1', '0') + ', ' +
                                                         iif(ReplayData[I].KeyChangeWeap, '1', '0') + ', ' +
                                                         iif(ReplayData[I].KeyThrow, '1', '0') + ', ' +
                                                         iif(ReplayData[I].KeyCrouch, '1', '0') + ', ' +
                                                         iif(ReplayData[I].KeyProne, '1', '0') + ', ' +
                                                         IntToStr(ReplayData[I].AimX) + ', ' +
                                                         IntToStr(ReplayData[I].AimY) + ', ' +
                                                         FloatToStr(ReplayData[I].PosX) + ', ' +
                                                         FloatToStr(ReplayData[I].PosY) + ')';
    QueryString := QueryString + ';';
    DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', QueryString);

    // Save CheckPoint data
    DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', DB_Query_Replace_Val2(SQL_DELETE_BESTRUN, Game.CurrentMap, runIDString));
    for I := 0 to RM.Map.AmountOfLaps - 1 do
      for J := 0 to RM.Map.AmountOfCheckPoints - 1 do
        DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', DB_Query_Replace_Val5(SQL_ADD_BESTRUN, Game.CurrentMap,
          runIDString, IntToStr(I + 1), IntToStr(J + 1), FormatDateTime('hh:nn:ss.zzz', RM.CurrentRunLap[I].Checkpoint[J])));

    DB_Close(DB_ID_REPLAYS);
    WriteLn('[RM] Finished... Database closed!');
  end else
  begin
    WriteLn('[RM] Could not open replay database!');
    Result := False;
  end;
end;

procedure LoadMapSettings(MapToLoad: string);
var
  I: Byte;
begin
  WriteLn('[RM] Starting to load map ' + MapToLoad + '...');
  RM.Map.Loaded := True;
  RM.BestRunLoaded := False;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, DB_Escape_String(MapToLoad))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      RM.Map.MapID               := DB_GetLong(DB_ID, 0); // `ID`
      RM.Map.AmountOfCheckpoints := DB_GetLong(DB_ID, 1); // `checknum`
      RM.Map.AmountOfLaps        := DB_GetLong(DB_ID, 2); // `roundnum`
      SetArrayLength(RM.Map.CheckPoints, RM.Map.AmountOfCheckpoints);

      DB_FinishQuery(DB_ID);

      SetArrayLength(RM.CurrentRunLap, RM.Map.AmountOfLaps);
      for I := 0 to RM.Map.AmountOfLaps - 1 do
        SetArrayLength(RM.CurrentRunLap[I].CheckPoint, RM.Map.AmountOfCheckpoints);

      if RM.Map.MapID < 0 then
      begin
        RM.Map.Loaded := False;
        WriteLnAndConsole(NIL, '[RM] Could not find settings for the map ' + MapToLoad + '!', MESSAGE_COLOR_GAME);
        Exit;
      end;

      if DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_CPS, IntToStr(RM.Map.MapID))) <> 0 then
      begin
        if DB_NextRow(DB_ID) = 0 then
        begin
          RM.Map.Loaded := False;
          WriteLn('[RM] The map ' + MapToLoad + ' has no checkpoints in settings!');
          DB_FinishQuery(DB_ID);
          Exit;
        end;

        for I := 5 to 254 do
          if Map.Spawns[I].Active then
            Map.Spawns[I].Active := False;

        Map.Spawns[TEAM_RUNNER].Active     := True;
        Map.Spawns[TEAM_VS].Active         := True;
        Map.Spawns[TEAM_EDITOR].Active     := True;
        Map.Spawns[TEAM_FREERUNNER].Active := True;
        Map.Spawns[TEAM_RUNNER].Style      := TEAM_RUNNER;
        Map.Spawns[TEAM_VS].Style          := TEAM_VS;
        Map.Spawns[TEAM_EDITOR].Style      := TEAM_EDITOR;
        Map.Spawns[TEAM_FREERUNNER].Style  := TEAM_FREERUNNER;

        RM.Map.RunnerStartSpawn.X     := DB_GetFloat(DB_ID, 1); // `posX`
        Map.Spawns[TEAM_RUNNER].X     := Trunc(RM.Map.RunnerStartSpawn.X);
        Map.Spawns[TEAM_VS].X         := Map.Spawns[TEAM_RUNNER].X;
        Map.Spawns[TEAM_EDITOR].X     := Map.Spawns[TEAM_VS].X;
        Map.Spawns[TEAM_FREERUNNER].X := Map.Spawns[TEAM_EDITOR].X;
        RM.Map.RunnerStartSpawn.Y     := DB_GetFloat(DB_ID, 2); // `posY`
        Map.Spawns[TEAM_RUNNER].Y     := Trunc(RM.Map.RunnerStartSpawn.Y);
        Map.Spawns[TEAM_VS].Y         := Map.Spawns[TEAM_RUNNER].Y;
        Map.Spawns[TEAM_EDITOR].Y     := Map.Spawns[TEAM_VS].Y;
        Map.Spawns[TEAM_FREERUNNER].Y := Map.Spawns[TEAM_EDITOR].Y;

        for I := 1 to 90 do
          if Map.Objects[I].Active then
            Map.Objects[I].Kill;

        for I := 0 to RM.Map.AmountOfCheckPoints - 1 do
          RM.Map.CheckPoints[I].Checked := True;

        while (DB_NextRow(DB_ID) <> 0) do
        begin
          try
            I := DB_GetLong(DB_ID, 0) - 1; // `checkpointID` - array is zero based

            RM.Map.CheckPoints[I].X        := DB_GetFloat(DB_ID, 1); // `posX`
            RM.Map.CheckPoints[I].Y        := DB_GetFloat(DB_ID, 2); // `posY`
            RM.Map.CheckPoints[I].Distance := DB_GetFloat(DB_ID, 3); // `distance`

            RM.Map.CheckPoints[I].Checked  := False;
          except
            WriteLn('[RM] Error: Database has more checkpoints saved');
            WriteLn('[RM] as in the map was defined! Please fix this.');
            WriteLn('[RM] CheckpointsNum: ' + IntToStr(RM.Map.AmountOfCheckpoints));
            WriteLn('[RM] Database returns checkpointID: ' + IntToStr(I + 1));
          end;
        end;

        LoadBestRun(GetBestRunIDOnMap(RM.Map.MapID));

        DB_FinishQuery(DB_ID);

        for I := 0 to RM.Map.AmountOfCheckPoints - 1 do
          if RM.Map.CheckPoints[I].Checked then
          begin
            RM.Map.Loaded := False;
            WriteLn('[RM] Could not load ' + MapToLoad + '! There are checkpoints missing (' + IntToStr(I + 1) + ')...');
            Break;
          end;

        if RM.Map.Loaded then
          WriteLn('[RM] The Map ' + MapToLoad + ' was loaded successfully!');

        WriteLn('[RM] Looking for a possible nextmap...');
        if DB_Query(DB_ID, SQL_GET_RND_MAP) <> 0 then
        begin
          if DB_NextRow(DB_ID) = 0 then
          begin
            WriteLn('[RM] Could not load any map! Set current map as next map!');
            RM.Map.NextMap := MapToLoad;
            DB_FinishQuery(DB_ID);
            Exit;
          end;

          RM.Map.NextMap := DB_GetString(DB_ID, 0); // `mapname`
          WriteLn('[RM] Next map will be ' + RM.Map.NextMap + '!');
          DB_FinishQuery(DB_ID);
        end;
      end else
      begin
        RM.Map.Loaded := False;
        WriteLn('[RM] Error in LoadMapSettings: ' + DB_Error());
        WriteLn('[RM] Error: The map ' + MapToLoad + ' could not been loaded!');
        DB_FinishQuery(DB_ID);
        Exit;
      end;
    end else
    begin
      RM.Map.Loaded := False;
      WriteLn('[RM] The map ' + MapToLoad + ' was not found in the Database! Please set it up.');
      WriteLn('[RM] Error in LoadMapSettings: ' + DB_Error());
      DB_FinishQuery(DB_ID);
      Exit;
    end;
  end else
  begin
    RM.Map.Loaded := False;
    WriteLn('[RM] Could not load the map ' + MapToLoad + '! Database is not connected!');
    Exit;
  end;
end;

function GetReplayBotID(): Byte;
var
  I: Byte;
begin
  Result := 0;
  for I := 1 to HighID do
    if Players[I].Active then
      if not Players[I].Human then
        if Players[I].Name = REPLAY_BOT_NAME then
          Result := I;
end;

function ReturnHandsWeapon(): TWeapon;
var
  NewHands: TNewWeapon;
begin
  NewHands := TNewWeapon.Create();
  try
    NewHands.WType := 255;
    NewHands.Ammo := 1;
    Result := NewHands;
  except
    Result := NIL;
    NewHands.Free;
  end;
end;

function AddReplayBot(): TActivePlayer;
var
  NewPlayer: TNewPlayer;
begin
  Result := NIL;
  NewPlayer := TNewPlayer.Create;
  try
    NewPlayer.Name                  := REPLAY_BOT_NAME;
    NewPlayer.Team                  := TEAM_SPECTATOR;
    NewPlayer.Dummy                 := True;
    NewPlayer.PantsColor            := REPLAY_BOT_COL_PANTS;
    NewPlayer.ShirtColor            := REPLAY_BOT_COL_SHIRT;
    NewPlayer.SkinColor             := REPLAY_BOT_COL_SKIN;
    NewPlayer.HairColor             := REPLAY_BOT_COL_HAIR;
    NewPlayer.HairStyle             := REPLAY_BOT_STY_HAIR;
    NewPlayer.Headgear              := REPLAY_BOT_STY_HEAD;
    NewPlayer.Chain                 := REPLAY_BOT_STY_CHAIN;
    NewPlayer.Primary               := ReturnHandsWeapon;
    NewPlayer.Secondary             := ReturnHandsWeapon;
    if (NewPlayer.Primary <> NIL) and (NewPlayer.Secondary <> NIL) then
      Result                          := Players.Add(NewPlayer);
  finally
    NewPlayer.Free;
  end;
end;

procedure DrawCheckPoints();
var
  i: Byte;
begin
  for i := 0 to RM.Map.AmountOfCheckpoints-1 do
    Players.WorldText(LAYER_OFFSET_CPS + i, iif(i + 1 = RM.Map.AmountOfCheckpoints,
      IntToStr(RM.Runner.Laps) + '/' + IntToStr(RM.Map.AmountOfLaps), IntToStr(i + 1)),
      MATH_SECOND_IN_TICKS * 10, iif(RM.Map.CheckPoints[i].Checked, MESSAGE_COLOR_GREEN,
      MESSAGE_COLOR_RED), 0.068, RM.Map.CheckPoints[i].X, RM.Map.CheckPoints[i].Y);
end;

procedure EndSingleGame(Successfull: Boolean);
var
  j: Byte;
  RunTime: TDateTime;
  Result_Run_ID: Integer;
begin
  RM.Active := False;
  if BotActive then
  begin
    CurrentLoop := 0;
    BotActive := False;
  end;
  for j := 0 to RM.Map.AmountOfCheckpoints-1 do
    RM.Map.CheckPoints[j].Checked := False;
  RM.Runner.PPlayer.Team := TEAM_SPECTATOR;
  if Successfull then
  begin
    RunTime := RM.CurrentRunLap[RM.Map.AmountOfLaps - 1].CheckPoint[RM.Map.AmountOfCheckPoints - 1];

    WriteLnAndConsole(NIL, '[RM] ' + RM.Runner.PPlayer.Name + ' has finished a run in ' + FormatDateTime('hh:nn:ss.zzz', RunTime), MESSAGE_COLOR_GAME);
    if ReplayBot <> NIL then
      if RM.Runner.PPlayer.ID <> ReplayBot.ID then
      begin
        WriteLnAndConsole(NIL, '[RM] Saving ' + RM.Runner.PPlayer.Name + '''s data.. This may take several seconds...', MESSAGE_COLOR_GAME);
        Game.OnClockTick := Pointers.Clock_Wait_Time;
        RM.Countdown := MATH_SECOND_IN_TICKS * 3;
        RM.Active := True;

        Result_Run_ID := Save_RunData(RM.Runner.PPlayer.HWID, RunTime);
        if Result_Run_ID > 0 then
        begin
          if Save_ReplayData(Result_Run_ID, ReplayValues) then
            WriteLn('[RM] Saved the replay!')
          else
            WriteLn('[RM] Failed to save the replay!');
          LoadBestRun(GetBestRunIDOnMap(RM.Map.MapID));
        end;
      end;
  end else
  begin
    WriteLnAndConsole(NIL, '[RM] ' + RM.Runner.PPlayer.Name + ' stopped his run.', MESSAGE_COLOR_GAME);
    DB_PerformConnectedQuery('EndSingleGame', DB_Query_Replace_Val1(SQL_INC_FAILSNUM, IntToStr(RM.Map.MapID)));
    Game.OnClockTick := Pointers.Clock_Wait_Time;
    RM.Countdown := MATH_SECOND_IN_TICKS * 1;
    RM.Active := True;
    DB_FinishQuery(DB_ID);
  end;
  RM.Runner.PPlayer := NIL;
end;

procedure PlayBot();
begin
  CurrentLoop := CurrentLoop + 1;
  if CurrentLoop = GetArrayLength(ReplayValues) then
  begin
    CurrentLoop := 0;
    BotActive := False;
    EndSingleGame(False);
    Exit;
  end;
  ReplayBot.KeyUp         := ReplayValues[CurrentLoop].KeyUp;
  ReplayBot.KeyLeft       := ReplayValues[CurrentLoop].KeyLeft;
  ReplayBot.KeyRight      := ReplayValues[CurrentLoop].KeyRight;
  ReplayBot.KeyJetpack    := ReplayValues[CurrentLoop].KeyJetpack;
  ReplayBot.KeyGrenade    := ReplayValues[CurrentLoop].KeyGrenade;
  ReplayBot.KeyChangeWeap := ReplayValues[CurrentLoop].KeyChangeWeap;
  ReplayBot.KeyThrow      := ReplayValues[CurrentLoop].KeyThrow;
  ReplayBot.KeyCrouch     := ReplayValues[CurrentLoop].KeyCrouch;
  ReplayBot.KeyProne      := ReplayValues[CurrentLoop].KeyProne;
  ReplayBot.MouseAimX     := ReplayValues[CurrentLoop].AimX;
  ReplayBot.MouseAimY     := ReplayValues[CurrentLoop].AimY;

  ReplayBot.Move(ReplayValues[CurrentLoop].PosX, ReplayValues[CurrentLoop].PosY);
end;

procedure RecordKeys();
var
  Len: Integer;
begin
  Len := GetArrayLength(ReplayValues);
  SetArrayLength(ReplayValues, Len + 1);
  ReplayValues[Len].KeyUp         := RM.Runner.PPlayer.KeyUp;
  ReplayValues[Len].KeyLeft       := RM.Runner.PPlayer.KeyLeft;
  ReplayValues[Len].KeyRight      := RM.Runner.PPlayer.KeyRight;
  ReplayValues[Len].KeyJetpack    := RM.Runner.PPlayer.KeyJetpack;
  ReplayValues[Len].KeyGrenade    := RM.Runner.PPlayer.KeyGrenade;
  ReplayValues[Len].KeyChangeWeap := RM.Runner.PPlayer.KeyChangeWeap;
  ReplayValues[Len].KeyThrow      := RM.Runner.PPlayer.KeyThrow;
  ReplayValues[Len].KeyCrouch     := RM.Runner.PPlayer.KeyCrouch;
  ReplayValues[Len].KeyProne      := RM.Runner.PPlayer.KeyProne;
  ReplayValues[Len].PosX          := RM.Runner.PPlayer.X;
  ReplayValues[Len].PosY          := RM.Runner.PPlayer.Y;
  ReplayValues[Len].AimX          := RM.Runner.PPlayer.MouseAimX;
  ReplayValues[Len].AimY          := RM.Runner.PPlayer.MouseAimY;
end;

procedure PassingCheckPoints();
var
i,j: byte;
Distances: Single;
RunTime: TDateTime;
begin
  for i := 0 to RM.Map.AmountOfCheckpoints-1 do
    if not RM.Map.CheckPoints[i].Checked then
    begin
      Distances := Distance(RM.Map.CheckPoints[i].X,RM.Map.CheckPoints[i].Y,RM.Runner.PPlayer.X,RM.Runner.PPlayer.Y);
      break;
    end;
  if Distances < RM.Map.CheckPoints[i].Distance then
  begin
    RunTime := Now - RM.Runner.StartTime;
    if i < RM.Map.AmountOfCheckpoints-1 then
    begin
      RM.Map.CheckPoints[i].Checked := True;
      Players.WorldText(0, FormatDateTime('hh:nn:ss.zzz', RunTime), MATH_SECOND_IN_TICKS * 2,
        MESSAGE_COLOR_GAME, 0.068, RM.Map.CheckPoints[i].X - 65, RM.Map.CheckPoints[i].Y + 50);
      RM.CurrentRunLap[RM.Runner.Laps].CheckPoint[i] := RunTime;
      if RM.BestRunLoaded and not BotActive then
        if RunTime > RM.BestRunLap[RM.Runner.Laps].CheckPoint[i] then
          Players.WorldText(1, '+' + FormatDateTime('hh:nn:ss.zzz', RunTime - RM.BestRunLap[RM.Runner.Laps].CheckPoint[i]), MATH_SECOND_IN_TICKS * 2,
            MESSAGE_COLOR_RED,   0.068, RM.Map.CheckPoints[i].X - 85, RM.Map.CheckPoints[i].Y + 70)
        else
          Players.WorldText(1, '-' + FormatDateTime('hh:nn:ss.zzz', RM.BestRunLap[RM.Runner.Laps].CheckPoint[i] - RunTime), MATH_SECOND_IN_TICKS * 2,
            MESSAGE_COLOR_GREEN, 0.068, RM.Map.CheckPoints[i].X - 75, RM.Map.CheckPoints[i].Y + 70);
      if i = 0 then
        RM.Map.CheckPoints[RM.Map.AmountOfCheckpoints-1].Checked := False;
      DrawCheckPoints;
    end
    else
    begin
      RM.Runner.Laps := RM.Runner.Laps + 1;
      for j := 0 to RM.Map.AmountOfCheckpoints-2 do
        RM.Map.CheckPoints[j].Checked := False;
      RM.Map.CheckPoints[RM.Map.AmountOfCheckpoints-1].Checked := True;
      DrawCheckPoints;
      RM.CurrentRunLap[RM.Runner.Laps - 1].CheckPoint[i] := RunTime;
      if RM.Runner.Laps = RM.Map.AmountOfLaps then
        EndSingleGame(True)
      else
      begin
        Players.WorldText(0, FormatDateTime('hh:nn:ss.zzz', RunTime), MATH_SECOND_IN_TICKS * 2,
          MESSAGE_COLOR_GAME, 0.068, RM.Map.CheckPoints[i].X - 65, RM.Map.CheckPoints[i].Y + 50);
        if RM.BestRunLoaded and not BotActive then
          if RunTime > RM.BestRunLap[RM.Runner.Laps - 1].CheckPoint[i] then
            Players.WorldText(1, '+' + FormatDateTime('hh:nn:ss.zzz', RunTime - RM.BestRunLap[RM.Runner.Laps - 1].CheckPoint[i]), MATH_SECOND_IN_TICKS * 2,
              MESSAGE_COLOR_RED,   0.068, RM.Map.CheckPoints[i].X - 85, RM.Map.CheckPoints[i].Y + 70)
          else
            Players.WorldText(1, '-' + FormatDateTime('hh:nn:ss.zzz', RM.BestRunLap[RM.Runner.Laps - 1].CheckPoint[i] - RunTime), MATH_SECOND_IN_TICKS * 2,
              MESSAGE_COLOR_GREEN, 0.068, RM.Map.CheckPoints[i].X - 75, RM.Map.CheckPoints[i].Y + 70);
      end;
    end;
  end;
end;

procedure CheckForReplayBot();
var
  i: Byte;
begin
  i := GetReplayBotID;
  if i = 0 then
    ReplayBot := AddReplayBot
  else
    ReplayBot := Players[i];
  BotActive := False;
  CurrentLoop := 0;
end;

function Medal_Color_by_Rank(Rank: Integer): Cardinal;
begin
  case Rank of
    1: Result := MESSAGE_COLOR_GOLD;
    2: Result := MESSAGE_COLOR_SILVER;
    3: Result := MESSAGE_COLOR_BRONZE;
    else
      Result := MESSAGE_COLOR_GAME;
  end;
end;

procedure ShowTop(TypedCommand: String);
var
  Text_Piece: TStringList;
  Top_X, SearchedMapID, RankID: Integer;
  PlayerName, SearchedMap, TotalRuns: String;
begin
  Text_Piece := File.CreateStringList();
  try
    SplitRegExpr(' ', TypedCommand, Text_Piece);
    if Length(Text_Piece.Strings[0]) = 4 then
      Top_X := 3
    else
      if Length(Text_Piece.Strings[0]) = 5 then
        Top_X := StrToInt(Text_Piece.Strings[0][5])
      else
        Top_X := 10;
    if Text_Piece.Count = 1 then
    begin
      SearchedMapID := RM.Map.MapID;
      SearchedMap   := Game.CurrentMap;
    end else
    begin
      if DB_CONNECTED then
      begin
        if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, DB_Escape_String(Text_Piece.Strings[1]))) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
        begin
          SearchedMapID := DB_GetLong(DB_ID, 0); // `ID`
          SearchedMap   := Text_Piece.Strings[1];
        end else
        begin
          Players.WriteConsole('[RM] Could not find the map ''' + Text_Piece.Strings[1] + '''!', MESSAGE_COLOR_RED);
          Exit;
        end;
      end else
      begin
        WriteLn('[RM] Could not find the map for ShowTop! Database is not connected!');
        Exit;
      end;
    end;

    if DB_CONNECTED then
    begin
      if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_TOP_X, IntToStr(SearchedMapID), IntToStr(Top_X))) <> 0) then
      begin
        RankID := 1;
        while DB_NextRow(DB_ID) <> 0 do
        begin
          // `rm_mapstats`.`ID` = 0 `rm_mapstats`.`playerID` = 1 `playerstats`.`name` = 2
          // `rm_mapstats`.`runtime` = 3 `rm_mapstats`.`rundate` = 4 `rm_maps`.`recordnum` = 5
          // `rm_maps`.`runsnum` = 6 `rm_maps`.`failsnum` = 7
          if RankID = 1 then
          begin
            TotalRuns := DB_GetString(DB_ID, 5);
            if StrToInt(TotalRuns) < Top_X then
              Top_X := StrToInt(TotalRuns);
            Players.WriteConsole('+-------------------------------------------------------------------------+', MESSAGE_COLOR_GAME);
            Players.WriteConsole('| Showing ' + IntToStr(Top_X) +' of ' + TotalRuns + ' recorded runs on map ''' +
              SearchedMap + '''! ' + WHITESPACES[23 - Length(SearchedMap)] + WHITESPACES[23 - Length(IntToStr(Top_X))] +
              WHITESPACES[23 - Length(TotalRuns)] +'                |', MESSAGE_COLOR_GOLD);
            Players.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);
            Players.WriteConsole('| Rank | Name                     | Time (H:M:S.ms) | Date (Y-M-D H:M:S)  |', MESSAGE_COLOR_GAME);
            Players.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);
          end;
          PlayerName := DB_GetString(DB_ID, 2);
          Players.WriteConsole('| #' + IntToStr(RankID) + WHITESPACES[20 + Length(IntToStr(RankID))] + ' | ' + PlayerName + WHITESPACES[Length(PlayerName) - 1] + ' | ' +
            DB_GetString(DB_ID, 3) + 's   | ' + DB_GetString(DB_ID, 4) + ' | [' + DB_GetString(DB_ID, 0) + ']', Medal_Color_by_Rank(RankID));
          RankID := RankID + 1;
        end;
        Players.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);

        DB_FinishQuery(DB_ID);

      end else
      begin
        WriteLn('[RM] The Map with the ID ' + IntToStr(SearchedMapID) + ' was not found in the Database!');
        WriteLn('[RM] Error in ShowTop: ' + DB_Error());
        DB_FinishQuery(DB_ID);
      end;
    end else
      WriteLn('[RM] Could not load the top! Database is not connected!');
  except
    WriteLn('[RM] Some error happened in ShowTop! Cannot figure out what...');
  finally
    Text_Piece.Free;
  end;
end;

procedure LoadReplay(p: TActivePlayer; ReplayTextID: string);
begin
  try
    ReplayValues := Explode_ReplayData(StrToInt(ReplayTextID));
  except
    WriteLnAndConsole(p, '[RM] ''' + ReplayTextID + ''' is not a valid number!', MESSAGE_COLOR_GAME);
    Exit;
  end;
  RM.Countdown := MATH_SECOND_IN_TICKS * 3;
  if GetArrayLength(ReplayValues) > 0 then
  begin
    RM.Active := True;
    Game.OnClockTick := Pointers.Clock_Load_Replay;
  end else
    WriteLnAndConsole(p, '[RM] No replay data was found for run ID ''' + ReplayTextID + '''!', MESSAGE_COLOR_GAME);
end;

procedure PerformSearch(p: TActivePlayer; Text: string);
var
  Text_Piece: TStringList;
  ResultString: string;
  ResID, ResGold, ResSilver, ResBronze: string;
begin
  Text_Piece := File.CreateStringList();
  try
    // Text_Piece.Strings[1] can be a map or player
    // Text_Piece.Strings[2] is the string you are searching for
    SplitRegExpr(' ', Text, Text_Piece);

    if Text_Piece.Count < 3 then
    begin
      p.WriteConsole('[RM] The command you have typed was incomplete (!search ''map''/''player'' <name>)!', MESSAGE_COLOR_RED);
      Exit;
    end;

    if DB_CONNECTED then
    begin
      case LowerCase(Text_Piece.Strings[1]) of
        'map':
        begin
          if Length(Text_Piece.Strings[2]) > 16 then
          begin
            p.WriteConsole('[RM] The map name you was searching for is too long!', MESSAGE_COLOR_RED);
            Exit;
          end;

          p.WriteConsole('+------------------------------+', MESSAGE_COLOR_GAME);
          p.WriteConsole('| Search map: ' + Text_Piece.Strings[2] + WHITESPACES[7 + Length(Text_Piece.Strings[2])] + ' |', MESSAGE_COLOR_GAME);
          p.WriteConsole('+------------------------------+', MESSAGE_COLOR_GAME);
          if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_MAP_BY_N, DB_Escape_String(Text_Piece.Strings[2]))) <> 0) AND
             (DB_NextRow(DB_ID) <> 0) then
          begin
            ResultString := DB_GetString(DB_ID, 0);  // `mapname`
            p.WriteConsole('| ' + ResultString + WHITESPACES[7 + Length(ResultString)] + '             |', MESSAGE_COLOR_GAME);
            while DB_NextRow(DB_ID) <> 0 do
            begin
              ResultString := DB_GetString(DB_ID, 0);  // `mapname`
              p.WriteConsole('| ' + ResultString + WHITESPACES[7 + Length(ResultString)] + '             |', MESSAGE_COLOR_GAME);
            end;
            p.WriteConsole('+------------------------------+', MESSAGE_COLOR_GAME);
            DB_FinishQuery(DB_ID);
          end else
          begin
            p.WriteConsole('| No map was found.            |', MESSAGE_COLOR_GAME);
            p.WriteConsole('+------------------------------+', MESSAGE_COLOR_GAME);
            DB_FinishQuery(DB_ID);
          end;
        end;
        'player':
        begin
          if Length(Text_Piece.Strings[2]) > 24 then
          begin
            p.WriteConsole('[RM] The player name you was searching for is too long!', MESSAGE_COLOR_RED);
            Exit;
          end;
          p.WriteConsole('+---------------------------------------------------------------+', MESSAGE_COLOR_GAME);
          p.WriteConsole('| Search player: ' + Text_Piece.Strings[2] + WHITESPACES[Length(Text_Piece.Strings[2]) - 1] +
            '                       |', MESSAGE_COLOR_GAME);
          p.WriteConsole('+---------+--------------------------+--------+--------+--------+', MESSAGE_COLOR_GAME);
          p.WriteConsole('| ID      | Name                     | Gold   | Silver | Bronze |', MESSAGE_COLOR_GAME);
          p.WriteConsole('+---------+--------------------------+--------+--------+--------+', MESSAGE_COLOR_GAME);
          if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_PLR_BY_N, DB_Escape_String(Text_Piece.Strings[2]))) <> 0) AND
             (DB_NextRow(DB_ID) <> 0) then
          begin
            ResID        := DB_GetString(DB_ID, 0); // `ID`
            ResultString := DB_GetString(DB_ID, 1); // `name`
            ResGold      := DB_GetString(DB_ID, 2); // `gold`
            ResSilver    := DB_GetString(DB_ID, 3); // `silver`
            ResBronze    := DB_GetString(DB_ID, 4); // `bronze`
            p.WriteConsole('| ' + ResID + WHITESPACES[16 + Length(ResID)] + ' | ' + ResultString + WHITESPACES[Length(ResultString) - 1] +
              ' | ' + ResGold + WHITESPACES[17 + Length(ResGold)] + ' | ' + ResSilver + WHITESPACES[17 + Length(ResSilver)] +
              ' | ' + ResBronze + WHITESPACES[17 + Length(ResBronze)] + ' |', MESSAGE_COLOR_GAME);
            while DB_NextRow(DB_ID) <> 0 do
            begin
              ResID        := DB_GetString(DB_ID, 0); // `ID`
              ResultString := DB_GetString(DB_ID, 1); // `name`
              ResGold      := DB_GetString(DB_ID, 2); // `gold`
              ResSilver    := DB_GetString(DB_ID, 3); // `silver`
              ResBronze    := DB_GetString(DB_ID, 4); // `bronze`
              p.WriteConsole('| ' + ResID + WHITESPACES[16 + Length(ResID)] + ' | ' + ResultString + WHITESPACES[Length(ResultString) - 1] +
                ' | ' + ResGold + WHITESPACES[17 + Length(ResGold)] + ' | ' + ResSilver + WHITESPACES[17 + Length(ResSilver)] +
                ' | ' + ResBronze + WHITESPACES[17 + Length(ResBronze)] + ' |', MESSAGE_COLOR_GAME);
            end;
            p.WriteConsole('+---------+--------------------------+--------+--------+--------+', MESSAGE_COLOR_GAME);
            DB_FinishQuery(DB_ID);
          end else
          begin
            p.WriteConsole('| No player was found.                                          |', MESSAGE_COLOR_GAME);
            p.WriteConsole('+---------------------------------------------------------------+', MESSAGE_COLOR_GAME);
            DB_FinishQuery(DB_ID);
          end;
        end;
        else
          p.WriteConsole('[RM] Please specify if you search a map or player! (!search ''map''/''player'' <name>)', MESSAGE_COLOR_RED);
      end;
    end else
      WriteLnAndConsole(p, '[RM] Could not perform a search! Database is not connected!', MESSAGE_COLOR_SYSTEM);
  except
    WriteLn('[RM] Some error happened in PerformSearch! Cannot figure out what...');
  finally
    Text_Piece.Free;
  end;
end;

procedure ShowQueue();
var
  I: Byte;
begin
  if IsQueueEmpty() then
    Players.WriteConsole('[RM] The queue is empty!', MESSAGE_COLOR_GAME)
  else
  begin
    Players.WriteConsole('+--------------------------+', MESSAGE_COLOR_GAME);
    Players.WriteConsole('| Players in queue         |', MESSAGE_COLOR_GAME);
    Players.WriteConsole('+--------------------------+', MESSAGE_COLOR_GAME);
    for I := 1 to Queue.Tail do
      Players.WriteConsole('| ' + Players[Queue.Members[I]].Name +
        WHITESPACES[Length(Players[Queue.Members[I]].Name) - 1] + ' |', Medal_Color_by_Rank(I));
    Players.WriteConsole('+--------------------------+', MESSAGE_COLOR_GAME);
  end;
end;

procedure ShowStatistics();
begin
  if DB_CONNECTED then
  begin
    DB_FinishQuery(DB_ID);
    if (DB_Query(DB_ID, SQL_GET_STATISTICS) <> 0) AND
       (DB_NextRow(DB_ID) <> 0) then
    begin
      Players.WriteConsole('+-------------------+---------------------------------------------+', MESSAGE_COLOR_GAME);
      Players.WriteConsole('| Server statistics |        RunMode 3 created by ExHunter        |', MESSAGE_COLOR_GAME);
      Players.WriteConsole('+-------------------+---------------------------------------------+'     + #13#10 +
                           '|                                                                 |', MESSAGE_COLOR_GAME);
      Players.WriteConsole('  We have in total ' + DB_GetString(DB_ID, 0) + ' recorded runs.'        + #13#10 + // totalruns
                           '  Ran by ' + DB_GetString(DB_ID, 1) + ' different players.', MESSAGE_COLOR_GOLD);   // differentplayers
      Players.WriteConsole('|                                                                 |'     + #13#10 +
                           '|                                                                 |', MESSAGE_COLOR_GAME);
      Players.WriteConsole('  On ' + DB_GetString(DB_ID, 3) + ' different maps.'                     + #13#10 + // totalmaps
                           '  There are in total ' + DB_GetString(DB_ID, 2) + ' achievement points.',           // totalachievementpoints
                           MESSAGE_COLOR_GOLD);
      Players.WriteConsole('|                                                                 |'     + #13#10 +
                           '|                                                                 |', MESSAGE_COLOR_GAME);
      Players.WriteConsole('  In ' + DB_GetString(DB_ID, 4) + ' different achievements.'             + #13#10 + // totaldifferentachievements
                           '  There are already ' + DB_GetString(DB_ID, 6) + ' runs and ' + DB_GetString(DB_ID, 5) + ' fails made.',
                           MESSAGE_COLOR_GOLD);  // totaltries = 6 and totalfails = 5
      Players.WriteConsole('|                                                                 |', MESSAGE_COLOR_GAME);
      Players.WriteConsole('+-----------------------------------------------------------------+', MESSAGE_COLOR_GAME);
    end else
    begin
      WriteLnAndConsole(NIL, '[RM] Could not find the statistics!', MESSAGE_COLOR_SYSTEM);
      WriteLn('[RM] Error in ShowStatistics: ' + DB_Error());
    end;
    DB_FinishQuery(DB_ID);
  end else
    WriteLnAndConsole(NIL, '[RM] Could not show the statistics! Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure OnSpeak(p: TActivePlayer; Text: string);
begin
  if Text[1] = '!' then
    case LowerCase(ReplaceRegExpr(REGEXP_FIRST_WORD, Text, '', False)) of
      '!play':
      begin
        if not RM.Active and RM.Map.Loaded then
          if IsQueueEmpty() then
            p.Team := TEAM_RUNNER
          else
            if QueuePosition(p.ID) = 1 then
            begin
              p.Team := TEAM_RUNNER;
              ReQueue();
            end else
              p.WriteConsole('[RM] It''s not your turn yet! If you did not add yourself yet, do !add', MESSAGE_COLOR_RED);
      end;
      '!fail':
      begin
        if RM.Active then
          if p.ID = RM.Runner.PPlayer.ID then
            EndSingleGame(False);
      end;
      '!freerun': p.Team := TEAM_FREERUNNER;
      '!add':
      begin
        if QueuePosition(p.ID) = 0 then
        begin
          EnQueue(p.ID);
          p.WriteConsole('[RM] Successfully added to the queue!', MESSAGE_COLOR_GAME);
        end else
          p.WriteConsole('[RM] You are already in the queue!', MESSAGE_COLOR_RED);
      end;
      '!del':
      begin
        if QueuePosition(p.ID) > 0 then
        begin
          if RemoveFromQueue(p.ID) then
            p.WriteConsole('[RM] Successfully removed from the queue!', MESSAGE_COLOR_GAME)
          else
            p.WriteConsole('[RM] Could not remove you from the queue!', MESSAGE_COLOR_RED);
        end else
          p.WriteConsole('[RM] You are not in the queue!', MESSAGE_COLOR_RED);
      end;
      '!queue': ShowQueue;
      '!top': ShowTop(Text);
      '!top10': ShowTop(Text);
      '!statistics': ShowStatistics;
      '!hnseu':
      begin
        WriteLn('Forwarding ' + p.Name + ' to !Hide and Seek EU');
        Players.WriteConsole('Forwarding ' + p.Name + ' to !Hide and Seek EU. Type !hnsEU to follow', MESSAGE_COLOR_SYSTEM);
        p.ForwardTo('62.75.221.26', 23072, 'Redirecting to !Hide and Seek EU ...');
      end;
      '!hnsna':
      begin
        WriteLn('Forwarding ' + p.Name + ' to !Hide and Seek NA');
        Players.WriteConsole('Forwarding ' + p.Name + ' to !Hide and Seek NA. Type !hnsNA to follow', MESSAGE_COLOR_SYSTEM);
        p.ForwardTo('198.7.57.226', 23073, 'Redirecting to !Hide and Seek NA ...');
      end;
      '!rmeu':
      begin
        WriteLn('Forwarding ' + p.Name + ' to !RunMode EU');
        Players.WriteConsole('Forwarding ' + p.Name + ' to !RunMode EU. Type !rmEU to follow', MESSAGE_COLOR_SYSTEM);
        p.ForwardTo('62.75.221.26', 23074, 'Redirecting to !RunMode EU ...');
      end;
      '!rmna':
      begin
        WriteLn('Forwarding ' + p.Name + ' to !RunMode NA');
        Players.WriteConsole('Forwarding ' + p.Name + ' to !RunMode NA. Type !rmNA to follow', MESSAGE_COLOR_SYSTEM);
        p.ForwardTo('198.7.57.226', 23074, 'Redirecting to !RunMode NA ...');
      end;
      '!rzal':
      begin
        WriteLn('Forwarding ' + p.Name + ' to #Rzal [Climb]');
        Players.WriteConsole('Forwarding ' + p.Name + ' to #Rzal [Climb]. Type !rzal to follow', MESSAGE_COLOR_SYSTEM);
        p.ForwardTo('185.25.151.122', 23074, 'Redirecting to #Rzal [Climb] ...');
      end;
      '!replay':
      begin
        if not RM.Active and RM.Map.Loaded then
          LoadReplay(p, Copy(Text, 9, Length(Text)));
      end;
      '!search': PerformSearch(p, Text);
      else
        p.WriteConsole('[GAME] The command you have typed was invalid!', MESSAGE_COLOR_RED);
    end;
end;

procedure UniversalClockCalls(t: Integer);
begin
  if t mod (MATH_SECOND_IN_TICKS * 5) = 0 then
  begin
    if RM.Map.Loaded then
      DrawCheckPoints;
    if t mod (MATH_MINUTE_IN_TICKS * 15) = 0 then
      DB_Ping_Server;
  end;
end;

procedure OnIdleTick(t: Integer);
begin
  UniversalClockCalls(t);
  if RM.Active then
    if BotActive then
    begin
      PassingCheckPoints();
      PlayBot();
    end else
    begin
      RecordKeys();
      PassingCheckPoints();
    end;
end;

procedure WaitingForReplayLoad(t: Integer);
begin
  UniversalClockCalls(t);
  if RM.Countdown > 0 then
  begin
    if RM.Countdown mod MATH_SECOND_IN_TICKS = 0 then
      WriteLnAndConsole(NIL, '[RM] Replay starts in ' + IntToStr(RM.Countdown div MATH_SECOND_IN_TICKS) + ' second(s)...', MESSAGE_COLOR_GAME);
    RM.Countdown := RM.Countdown - 1;
  end else
  begin
    Game.OnClockTick := Pointers.Clock_Normal;
    RM.Active := False;
    BotActive := True;
    CurrentLoop := 0;
    ReplayBot.Team := TEAM_RUNNER;
    WriteLnAndConsole(NIL, '[RM] Replay has started...', MESSAGE_COLOR_GAME);
  end;
end;

procedure WaitForNextRun(t: Integer);
begin
  UniversalClockCalls(t);
  if RM.Countdown > 0 then
  begin
    if RM.Countdown mod MATH_SECOND_IN_TICKS = 0 then
      Players.WriteConsole('[RM] ' + IntToStr(RM.Countdown div MATH_SECOND_IN_TICKS) + '...',
        Medal_Color_by_Rank(RM.Countdown div MATH_SECOND_IN_TICKS));
    RM.Countdown := RM.Countdown - 1;
  end else
  begin
    Game.OnClockTick := Pointers.Clock_Normal;
    Players.WriteConsole('[RM] ' + IntToStr(RM.Countdown div MATH_SECOND_IN_TICKS) + '... Ready for next run!',
      MESSAGE_COLOR_GREEN);
    RM.Active := False;
  end;
end;

procedure AfterMapChange(NewMap: String);
begin
  LoadMapSettings(NewMap);
  if RM.Map.Loaded then
    DrawCheckPoints;
end;

function OnDamage(Shooter, Victim: TActivePlayer; Damage: Integer; BulletID: Byte): Integer;
begin
  Result := Damage;
end;

procedure OnKill(Killer, Victim: TActivePlayer; BulletID: Byte);
begin
  if RM.Active then
    if Victim.ID = RM.Runner.PPlayer.ID then
      EndSingleGame(False);
end;

procedure EndGameAfterRespawn(p: TActivePlayer);
begin
  if RM.Active then
    if p.ID = RM.Runner.PPlayer.ID then
      EndSingleGame(False);
end;

procedure UpdatePlayerDatabase(p: TActivePlayer);
var
  PlayerName: string;
  PlayerIP: string;
begin
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_PLAYER_JOIN, p.HWID)) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      PlayerName := DB_GetString(DB_ID, 0); // `name`
      if DB_GetLong(DB_ID, 1) > 0 then      // `adm`
        p.IsAdmin := True;
      PlayerName := DB_GetString(DB_ID, 2); // `lastip`
      DB_FinishQuery(DB_ID);

      if (PlayerName <> p.Name) or (PlayerIP <> p.IP) then
      begin
        DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val3(SQL_UPDATE_PLR_JOIN, DB_Escape_String(p.Name), p.IP, p.HWID));
        DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val4(SQL_LOG_NAMECHANGE, p.HWID, p.IP, IntToStr(DB_SERVER_ID), DB_Escape_String(p.Name)));
      end;

      DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val1(SQL_UPDATE_PLR_SEEN, p.HWID));
    end else
    begin
      WriteLn('[RM] Player with HWID ' + p.HWID + ' was not found in Database...');
      DB_FinishQuery(DB_ID);

      WriteLn('[RM] Adding ' + p.Name + ' to the Database...');
      DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val3(SQL_ADD_PLAYER, p.HWID, DB_Escape_String(p.Name), p.IP));
      DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val4(SQL_LOG_NAMECHANGE, p.HWID, p.IP, IntToStr(DB_SERVER_ID), DB_Escape_String(p.Name)));
    end;
  end else
    WriteLn('[RM] Could not check for the player! Database is not connected!');
end;

procedure GameOnJoin(p: TActivePlayer; Team: TTeam);
begin
  if p.ID > HighID then
    HighID := p.ID;
  if Team.ID = TEAM_RUNNER then
    if RM.Active then
      if p.ID = RM.Runner.PPlayer.ID then
        RM.Active := False;
  if p.Human then
  begin
    if RM.Map.Loaded then
      DrawCheckPoints;
    UpdatePlayerDatabase(p); // Adds the player if not in Database
    p.WriteConsole('[HELP] Welcome to !RunMode. Type !help if you are new.', MESSAGE_COLOR_SYSTEM);
  end;
  if p.Team <> TEAM_SPECTATOR then
    p.Team := TEAM_SPECTATOR;
end;

procedure GameOnLeave(p: TActivePlayer; Kicked: Boolean);
var
  I: Byte;
begin
  if p.IsAdmin then
    p.IsAdmin := False;
  if HighID = p.ID then
    for I := HighID - 1 downto 1 do
      if Players[I].Active then
      begin
        HighID := I;
        break;
      end;
end;

procedure OnJoinTeamInvalid(p: TActivePlayer; Team: TTeam);
begin
  if RM.Active then
    if p.ID = RM.Runner.PPlayer.ID then
    begin
      EndSingleGame(False);
      Exit;
    end;
  p.WriteConsole('[GAME] You cannot join this Team.', MESSAGE_COLOR_GAME);
  p.Team := TEAM_SPECTATOR;
end;

procedure OnJoinTeamRunner(p: TActivePlayer; Team: TTeam);
begin
  if RM.Active then
    if p.ID = RM.Runner.PPlayer.ID then
    begin
      EndSingleGame(False);
      Exit;
    end;
  if not RM.Active and RM.Map.Loaded then
  begin
    if ReplayBot <> NIL then
    begin
      if ReplayBot.ID <> p.ID then
      begin
        SetArrayLength(ReplayValues, 0);
        RM.Runner.StartTime := Now();
      end else
        // Workaround since bots have always 1 tick more in counter
        RM.Runner.StartTime := Now() - StrToDateTime('00:00:00.016');
    end else
      RM.Runner.StartTime := Now();
    RM.Runner.PPlayer := p;
    RM.Runner.Laps := 0;
    RM.Active := True;
  end
  else
    OnJoinTeamInvalid(p, Team);
end;

procedure OnJoinTeamVersus(p: TActivePlayer; Team: TTeam);
begin
  // TODO: IMPLEMENT
  OnJoinTeamInvalid(p, Team);
end;

procedure OnJoinTeamEditor(p: TActivePlayer; Team: TTeam);
begin
  if RM.Active then
    if p.ID = RM.Runner.PPlayer.ID then
    begin
      EndSingleGame(False);
      Exit;
    end;
  if not p.IsAdmin then
    OnJoinTeamInvalid(p, Team);
end;

procedure OnJoinTeamFreeRunner(p: TActivePlayer; Team: TTeam);
begin
  if RM.Active then
    if p.ID = RM.Runner.PPlayer.ID then
    begin
      EndSingleGame(False);
      Exit;
    end;
  p.WriteConsole('[GAME] You are now freerunning.', MESSAGE_COLOR_GAME);
end;

procedure OnJoinTeamSpectator(p: TActivePlayer; Team: TTeam);
begin
  if RM.Active then
    if p.ID = RM.Runner.PPlayer.ID then
    begin
      EndSingleGame(False);
      Exit;
    end;
  if p.Human then
    p.WriteConsole('[GAME] You are now spectating. Type !play or !freerun to play.', MESSAGE_COLOR_GAME);
end;

function ErrorHandler(ErrorCode: TErrorType; Message, UnitName, FunctionName: string; Row, Col: Cardinal): Boolean;
begin
  WriteLn('-------------------------');
  WriteLn('[ERROR-UNIT] ' + UnitName);
  WriteLn('[ERROR-FUNC] ' + FunctionName);
  WriteLn('[ERROR-POS ] ' + IntToStr(Col) + ' - ' + IntToStr(Row));
  WriteLn('[ERROR-MSG ] ' + Message);
  WriteLn('-------------------------');
  result := True;
end;

procedure AddMapToDatabase(p: TActivePlayer);
begin
  if IsInEditorMode(p) then
  begin
    if DB_CONNECTED then
    begin
      if DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, Game.CurrentMap)) <> 0 then
      begin
        if DB_NextRow(DB_ID) <> 0 then
          p.WriteConsole('[RM] The current map is already in the Database!', MESSAGE_COLOR_RED)
        else
        begin
          if DB_Open(DB_ID_REPLAYS, DB_CON_STRING_REPLAY, '', '', DB_Plugin_ODBC) <> 0 then
          begin
            DB_PerformQuery(DB_ID, 'AddMapToDatabase', DB_Query_Replace_Val1(SQL_ADD_MAP, Game.CurrentMap));
            DB_PerformQuery(DB_ID_REPLAYS, 'AddMapToDatabase', DB_Query_Replace_Val1(SQL_CREATE_REPLAY_TBL, Game.CurrentMap));
            DB_PerformQuery(DB_ID_REPLAYS, 'AddMapToDatabase', DB_Query_Replace_Val1(SQL_CREATE_BESTRUN, Game.CurrentMap));
            p.WriteConsole('[RM] Added ' + Game.CurrentMap + ' to the Database!', MESSAGE_COLOR_RED);
          end else
            WriteLnAndConsole(p, '[RM] Error in AddMapToDatabase: ' + DB_Error(), MESSAGE_COLOR_RED);
          DB_FinishQuery(DB_ID_REPLAYS);
          DB_Close(DB_ID_REPLAYS);
        end;
      end else
        WriteLnAndConsole(p, '[RM] Error in AddMapToDatabase: ' + DB_Error(), MESSAGE_COLOR_RED);
      DB_FinishQuery(DB_ID);
    end else
      WriteLnAndConsole(p, '[RM] Error: The server is not connected to the Database!', MESSAGE_COLOR_RED);
  end else
    p.WriteConsole('[RM] You have to be in the Editor mode to add a map!', MESSAGE_COLOR_RED);
end;

procedure AddCPToDatabase(p: TActivePlayer; Command: string);
var
  Text_Piece: TStringList;
  MapID: Integer;
begin
  if IsInEditorMode(p) then
  begin
    if DB_CONNECTED then
    begin
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, Game.CurrentMap)) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
      begin
        MapID := DB_GetLong(DB_ID, 0); // `ID`
        DB_FinishQuery(DB_ID);
        Text_Piece := File.CreateStringList();
        try
          SplitRegExpr(' ', Command, Text_Piece);
          if Text_Piece.Count > 2 then
          begin
            // Text_Piece[1] = CPNum, Text_Piece[2] = CheckDistance
            // IntToStr(StrToInt(Text_Piece[x])) to check if it's a number
            if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_CP_ID, IntToStr(MapID), IntToStr(StrToInt(Text_Piece[1])))) <> 0) and
               (DB_NextRow(DB_ID) <> 0) then
            begin
              DB_FinishQuery(DB_ID);
              DB_PerformQuery(DB_ID, 'AddCPToDatabase', DB_Query_Replace_Val5(SQL_UPDATE_CP, FloatToStr(p.X), FloatToStr(p.Y),
                IntToStr(StrToInt(Text_Piece[2])), IntToStr(MapID), Text_Piece[1]));
              WriteLnAndConsole(p, '[RM] Successfully updated CP ' + Text_Piece[1] + '!', MESSAGE_COLOR_SYSTEM);
            end else
            begin
              DB_FinishQuery(DB_ID);
              DB_PerformQuery(DB_ID, 'AddCPToDatabase', DB_Query_Replace_Val5(SQL_ADD_CP, IntToStr(MapID), Text_Piece[1],
                FloatToStr(p.X), FloatToStr(p.Y), IntToStr(StrToInt(Text_Piece[2]))));
              WriteLnAndConsole(p, '[RM] Successfully added CP ' + Text_Piece[1] + '!', MESSAGE_COLOR_SYSTEM);
            end;
          end else
            p.WriteConsole('[RM] Please specify a checkpoint ID and distance check! /addcp <id> <distance>', MESSAGE_COLOR_RED);
        except
          WriteLnAndConsole(p, '[RM] Some error happened in AddCPToDatabase! Cannot figure out what...', MESSAGE_COLOR_RED);
        finally
          Text_Piece.Free;
        end;
      end else
      begin
        WriteLnAndConsole(p, '[RM] The map ' + Game.CurrentMap + ' was not found in the Database! Please add it before adding checkpoints.', MESSAGE_COLOR_RED);
        DB_FinishQuery(DB_ID);
      end;
    end else
      WriteLnAndConsole(p, '[RM] Could not load the map ' + Game.CurrentMap + '! Database is not connected!', MESSAGE_COLOR_RED);
  end else
    p.WriteConsole('[RM] You have to be in the Editor mode to add a map!', MESSAGE_COLOR_RED);
end;

procedure SetLapsToDatabase(p: TActivePlayer; Command: string);
var
  Text_Piece: TStringList;
  MapID: Integer;
begin
  if IsInEditorMode(p) then
  begin
    if DB_CONNECTED then
    begin
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, Game.CurrentMap)) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
      begin
        MapID := DB_GetLong(DB_ID, 0); // `ID`
        DB_FinishQuery(DB_ID);
        Text_Piece := File.CreateStringList();
        try
          SplitRegExpr(' ', Command, Text_Piece);
          if Text_Piece.Count > 1 then
          begin
            // Text_Piece[1] = LapsNum
            // IntToStr(StrToInt(Text_Piece[x])) to check if it's a number
            DB_PerformQuery(DB_ID, 'SetLapsToDatabase', DB_Query_Replace_Val2(SQL_SET_LAPS,
              IntToStr(StrToInt(Text_Piece[1])), IntToStr(MapID)));
            WriteLnAndConsole(p, '[RM] Successfully updated Laps num to ' + Text_Piece[1] + '!', MESSAGE_COLOR_SYSTEM);
          end else
            p.WriteConsole('[RM] Please specify an amount of laps! /setlaps <laps>', MESSAGE_COLOR_RED);
        except
          WriteLnAndConsole(p, '[RM] Some error happened in SetLapsToDatabase! Cannot figure out what...', MESSAGE_COLOR_RED);
        finally
          Text_Piece.Free;
        end;
      end else
      begin
        WriteLnAndConsole(p, '[RM] The map ' + Game.CurrentMap + ' was not found in the Database! Please add it before modifying laps.', MESSAGE_COLOR_RED);
        DB_FinishQuery(DB_ID);
      end;
    end else
      WriteLnAndConsole(p, '[RM] Could not load the map ' + Game.CurrentMap + '! Database is not connected!', MESSAGE_COLOR_RED);
  end else
    p.WriteConsole('[RM] You have to be in the Editor mode to modify laps!', MESSAGE_COLOR_RED);
end;

procedure SetCPNumToDatabase(p: TActivePlayer; Command: string);
var
  Text_Piece: TStringList;
  MapID: Integer;
begin
  if IsInEditorMode(p) then
  begin
    if DB_CONNECTED then
    begin
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, Game.CurrentMap)) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
      begin
        MapID := DB_GetLong(DB_ID, 0); // `ID`
        DB_FinishQuery(DB_ID);
        Text_Piece := File.CreateStringList();
        try
          SplitRegExpr(' ', Command, Text_Piece);
          if Text_Piece.Count > 1 then
          begin
            // Text_Piece[1] = CheckPointsNum
            // IntToStr(StrToInt(Text_Piece[x])) to check if it's a number
            DB_PerformQuery(DB_ID, 'SetCPNumToDatabase', DB_Query_Replace_Val2(SQL_SET_CPS,
              IntToStr(StrToInt(Text_Piece[1])), IntToStr(MapID)));
            WriteLnAndConsole(p, '[RM] Successfully updated checkpoints count to ' + Text_Piece[1] + '!', MESSAGE_COLOR_SYSTEM);
          end else
            p.WriteConsole('[RM] Please specify an amount of checkpoints! /setcpnum <count>', MESSAGE_COLOR_RED);
        except
          WriteLnAndConsole(p, '[RM] Some error happened in SetCPNumToDatabase! Cannot figure out what...', MESSAGE_COLOR_RED);
        finally
          Text_Piece.Free;
        end;
      end else
      begin
        WriteLnAndConsole(p, '[RM] The map ' + Game.CurrentMap + ' was not found in the Database! Please add it before modifying checkpoints.', MESSAGE_COLOR_RED);
        DB_FinishQuery(DB_ID);
      end;
    end else
      WriteLnAndConsole(p, '[RM] Could not load the map ' + Game.CurrentMap + '! Database is not connected!', MESSAGE_COLOR_RED);
  end else
    p.WriteConsole('[RM] You have to be in the Editor mode to modify checkpoints!', MESSAGE_COLOR_RED);
end;

procedure RemoveCPFromDatabase(p: TActivePlayer; Command: string);
var
  Text_Piece: TStringList;
  MapID: Integer;
begin
  if IsInEditorMode(p) then
  begin
    if DB_CONNECTED then
    begin
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, Game.CurrentMap)) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
      begin
        MapID := DB_GetLong(DB_ID, 0); // `ID`
        DB_FinishQuery(DB_ID);
        Text_Piece := File.CreateStringList();
        try
          SplitRegExpr(' ', Command, Text_Piece);
          if Text_Piece.Count > 1 then
          begin
            // Text_Piece[1] = CheckPointsNum
            // IntToStr(StrToInt(Text_Piece[x])) to check if it's a number
            DB_PerformQuery(DB_ID, 'RemoveCPFromDatabase', DB_Query_Replace_Val2(SQL_DEL_CP,
              IntToStr(MapID), IntToStr(StrToInt(Text_Piece[1]))));
            WriteLnAndConsole(p, '[RM] Successfully deleted checkpoint ' + Text_Piece[1] + '!', MESSAGE_COLOR_SYSTEM);
          end else
            p.WriteConsole('[RM] Please specify the checkpoint ID! /delcp <id>', MESSAGE_COLOR_RED);
        except
          WriteLnAndConsole(p, '[RM] Some error happened in RemoveCPFromDatabase! Cannot figure out what...', MESSAGE_COLOR_RED);
        finally
          Text_Piece.Free;
        end;
      end else
      begin
        WriteLnAndConsole(p, '[RM] The map ' + Game.CurrentMap + ' was not found in the Database! Please add it before removing a checkpoint.', MESSAGE_COLOR_RED);
        DB_FinishQuery(DB_ID);
      end;
    end else
      WriteLnAndConsole(p, '[RM] Could not load the map ' + Game.CurrentMap + '! Database is not connected!', MESSAGE_COLOR_RED);
  end else
    p.WriteConsole('[RM] You have to be in the Editor mode to remove a checkpoint!', MESSAGE_COLOR_RED);
end;

procedure AdminChat(p: TActivePlayer; Command: string);
var
  I: Byte;
begin
  if p = NIL then
  begin
    for I := 1 to HighID do
      if Players[I].IsAdmin then
        Players[I].WriteConsole(Copy(Command, 2, Length(Command) - 1), MESSAGE_COLOR_AC);
  end else
    if p.IsAdmin then
      if Length(Command) > 4 then
      begin
        WriteLn(' <' + p.Name + '> ' + copy(Command, 5, Length(Command)));
        for I := 1 to HighID do
          if Players[I].IsAdmin then
            Players[I].WriteConsole('[' + p.Name + '] ' + Copy(Command, 5, Length(Command) - 4), MESSAGE_COLOR_AC);
      end;
end;

procedure LookUpPlayerOrHWID(p: TActivePlayer; Command: string);
var
  Text_Piece: TStringList;
  LookUpString: string;
  ResultNameRow: string;
  ResultCountRow: string;
begin
  Text_Piece := File.CreateStringList();
  try
    SplitRegExpr(' ', Command, Text_Piece);

    if Text_Piece.Count < 3 then
    begin
      DecideIfWriteLnOrConsole(p, '[RM] The command you have typed was incomplete (/lookup ''hwid''/''id'' <value>)!', MESSAGE_COLOR_RED);
      Exit;
    end;

  if LowerCase(Text_Piece[1]) = 'hwid' then
  begin
    LookUpString := DB_Escape_String(Text_Piece[2]);
    if Length(LookUpString) <> 11 then
    begin
      DecideIfWriteLnOrConsole(p, '[RM] The HWID you''ve typed in was too long or short! It has to be 11 chars long!', MESSAGE_COLOR_RED);
      Exit;
    end;
  end
  else
    if LowerCase(Text_Piece[1]) = 'id' then
    begin
      if Players[StrToInt(Text_Piece[2])].Active then
        LookUpString := Players[StrToInt(Text_Piece[2])].HWID
      else
      begin
        DecideIfWriteLnOrConsole(p, '[RM] The command you have typed was incorrect (/lookup id <ACTIVE PLAYER ID>)!', MESSAGE_COLOR_RED);
        Exit;
      end;
    end else
    begin
      DecideIfWriteLnOrConsole(p, '[RM] The command you have typed was incorrect (/lookup ''hwid''/''id'' <value>)!', MESSAGE_COLOR_RED);
      Exit;
    end;

  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_ALT_NAME, LookUpString)) <> 0) then
    begin
      ResultNameRow  := DB_GetString(DB_ID, 0); // `info`
      ResultCountRow := DB_GetString(DB_ID, 1); //  cnt
      DecideIfWriteLnOrConsole(p, '+------------------------------------------+', MESSAGE_COLOR_SYSTEM);
      DecideIfWriteLnOrConsole(p, '| Showing the results of ' + LookUpString + ':      |', MESSAGE_COLOR_SYSTEM);
      DecideIfWriteLnOrConsole(p, '+--------------------------+---------------+', MESSAGE_COLOR_SYSTEM);
      DecideIfWriteLnOrConsole(p, '| Name                     | changed times |', MESSAGE_COLOR_SYSTEM);
      DecideIfWriteLnOrConsole(p, '+--------------------------+---------------+', MESSAGE_COLOR_SYSTEM);
      while DB_NextRow(DB_ID) <> 0 do
      begin
        ResultNameRow  := DB_GetString(DB_ID, 0); // `info`
        ResultCountRow := DB_GetString(DB_ID, 1); //  cnt
        DecideIfWriteLnOrConsole(p, '| ' + ResultNameRow + WHITESPACES[Length(ResultNameRow) - 1] + ' | ' +
          ResultCountRow + WHITESPACES[10 + Length(ResultCountRow)] + ' |', MESSAGE_COLOR_SYSTEM);
      end;
      DecideIfWriteLnOrConsole(p, '+--------------------------+---------------+', MESSAGE_COLOR_SYSTEM);
      DB_FinishQuery(DB_ID);
    end else
    begin
      DecideIfWriteLnOrConsole(p, '[RM] Did not find anything in logs for the specific player!', MESSAGE_COLOR_RED);
      DecideIfWriteLnOrConsole(p, '[RM] Error in LookUpPlayerOrHWID: ' + DB_Error(), MESSAGE_COLOR_RED);
    end;
  end else
    DecideIfWriteLnOrConsole(p, '[RM] Could not lookup the player! Database is not connected!', MESSAGE_COLOR_RED);

  except
    DecideIfWriteLnOrConsole(p, '[RM] Some error happened in LookUpPlayerOrHWID! Cannot figure out what...', MESSAGE_COLOR_RED);
  finally
    Text_Piece.Free;
  end;
end;

function OnSharedAdminCommand(p: TActivePlayer; Command: string): Boolean;
begin
  Result := False;
  case LowerCase(ReplaceRegExpr(REGEXP_FIRST_WORD, Command, '', False)) of
    '/say':
    begin
      Result := True;
      Players.WriteConsole(Copy(Command, 6, Length(Command) - 5), MESSAGE_COLOR_GREEN);
    end;
    '/as':
    begin
      if p = NIL then
        Players.WriteConsole('[ADMIN] ' + Copy(Command, 5, Length(Command) - 4),
          MESSAGE_COLOR_GREEN)
      else
        Players.WriteConsole('[' + p.Name + '] ' + Copy(Command, 5, Length(Command) - 4),
          MESSAGE_COLOR_GREEN);
    end;
    '/lookup': LookUpPlayerOrHWID(p, Command);
    else
      Result := False;
  end;
  // After setting Result := True it won't be shown anymore. This is a workaround
  if Result then
    if p <> NIL then
      WriteLn('BLOCKED COMMAND: ' + Command + '(' + p.IP + '[' + p.Name + '])');
end;

function OnInGameAdminCommand(p: TActivePlayer; Command: string): Boolean;
begin
  Result := False;
  case LowerCase(ReplaceRegExpr(REGEXP_FIRST_WORD, Command, '', False)) of
    '/ac':
    begin
      Result := True;
      AdminChat(p, Command);
    end;
    '/edit':     p.Team := TEAM_EDITOR;
    '/addmap':   AddMapToDatabase(p);
    '/addcp':    AddCPToDatabase(p, Command);
    '/setlaps':  SetLapsToDatabase(p, Command);
    '/setcpnum': SetCPNumToDatabase(p, Command);
    '/delcp':    RemoveCPFromDatabase(p, Command);
    '/teleport':
    begin
      if IsInEditorMode(p) then
        p.Move(p.MouseAimX, p.MouseAimY)
      else
        p.WriteConsole('[RM] You have to be in the Editor mode to teleport yourself!', MESSAGE_COLOR_SYSTEM);
    end;
    else
      Result := OnSharedAdminCommand(p, Command);
  end;
end;

function OnTCPAdminCommand(Ip: string; Port: Word; Command: string): Boolean;
begin
  Result := OnSharedAdminCommand(NIL, Command);
end;

procedure OnTCPAdminMessage(Ip: string; Port: Word; Msg: string);
begin
  if Msg[1] = ' ' then
    AdminChat(NIL, Msg);
end;

// Ignore BanLists. Use Database.
function RequestHandlerBan(Ip, Hw: string; Port: Word; State: Byte; Forwarded: Boolean; Password: string): Integer;
begin
  if State = REQUEST_STATE_BANNED then
    State := REQUEST_STATE_OK;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_ACTIVE_BANS, Hw, Ip)) <> 0) then
    begin
      if DB_NextRow(DB_ID) <> 0 then
      begin
        WriteLn('[RM] The requesting player is banned!');
        WriteLn('[RM] Reason: ' + DB_GetString(DB_ID, 2)); // `reason`
        WriteLn('[RM] Until:  ' + DB_GetString(DB_ID, 1)); // `until`
        WriteLn('[RM] Date:   ' + DB_GetString(DB_ID, 0)); // `date`
        WriteLn('[RM] By:     ' + DB_GetString(DB_ID, 3)); // `admin`
        State := REQUEST_STATE_BANNED;
      end else
        WriteLn('[RM] The requesting player is not banned!');
    end else
      WriteLn('[RM] Error in RequestHandlerBan: ' + DB_Error());
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Could not check for player bans! Database is not connected!');
  Result := State;
end;

procedure SetupRM();
var
  i: Byte;
begin
  WriteLn('[RM] Setting up RunMode3...');
  Pointers.Clock_Normal := @OnIdleTick;
  Pointers.Clock_Load_Replay := @WaitingForReplayLoad;
  Pointers.Clock_Wait_Time := @WaitForNextRun;
  DB_Establish_Connection;
  if Game.TickThreshold <> 1 then
    Game.TickThreshold := 1;
  Game.OnClockTick                   := @OnIdleTick;
  Game.OnJoin                        := @GameOnJoin;
  Game.OnLeave                       := @GameOnLeave;
  Game.Teams[0].OnJoin               := @OnJoinTeamInvalid;
  Game.Teams[TEAM_EDITOR].OnJoin     := @OnJoinTeamEditor;
  Game.Teams[TEAM_VS].OnJoin         := @OnJoinTeamVersus;
  Game.Teams[TEAM_RUNNER].OnJoin     := @OnJoinTeamRunner;
  Game.Teams[TEAM_FREERUNNER].OnJoin := @OnJoinTeamFreeRunner;
  Game.Teams[TEAM_SPECTATOR].OnJoin  := @OnJoinTeamSpectator;
  Map.OnAfterMapChange               := @AfterMapChange;
  Game.OnAdminCommand                := @OnInGameAdminCommand;
  Game.OnTCPCommand                  := @OnTCPAdminCommand;
  Game.OnTCPMessage                  := @OnTCPAdminMessage;
  Game.OnRequest                     := @RequestHandlerBan;
  Script.OnException                 := @ErrorHandler;
  HighID := 1;
  for i := 1 to 32 do
  begin
    Players[i].OnDamage              := @OnDamage;
    Players[i].OnSpeak               := @OnSpeak;
    Players[i].OnKill                := @OnKill;
    Players[i].OnAfterRespawn        := @EndGameAfterRespawn;
   // Players[i].OnCommand             := @OnPCMD;
    //Players[i].OnVoteMapStart        := @TestVoteMap;
  //HnS defines
    if Players[i].Active then
      HighID := i;
  end;
  WHITESPACES := ['                       ',
                  '                      ',
                  '                     ',
                  '                    ',
                  '                   ',
                  '                  ',
                  '                 ',
                  '                ',
                  '               ',
                  '              ',
                  '             ',
                  '            ',
                  '           ',
                  '          ',
                  '         ',
                  '        ',
                  '       ',
                  '      ',
                  '     ',
                  '    ',
                  '   ',
                  '  ',
                  ' ',
                  ''];
  CheckForReplayBot;
  WriteLn('[RM] RunMode3 setup finished. Script runs now...');
  // on server startup this is empty, because map is not loaded yet.
  // used for recompile instead of restart map.
  if Game.CurrentMap <> '' then
    LoadMapSettings(Game.CurrentMap);
end;

initialization

  SetupRM;

end.