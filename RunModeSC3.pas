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
  constants, libdb, achievements;

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
    LastDistances: Array[1..60] of Single;
    LastPos: TVector;
    CP: Byte;
    PositionArrayDistance: SmallInt;
    SumArrayDistance: Single;
    end;

  TGameVariables = record
    Countdown, Seconds: Integer;
    Active: Boolean;
    Map: TMapVariables;
    BestRunLap: Array of TBestRun;
    BestRunName: string;
    BestRunLoaded: Boolean;
    CurrentRunLap: Array of TBestRun;
    Runner: TRunnerProperties;
    TimeLeft: TDateTime;
    end;

  TReplay = record
    KeyMask: Word;
    AimX, AimY: SmallInt;
    PosX, PosY: Single;
    VelX, VelY: Single;
    end;

  TPointers = record
    Clock_Normal, Clock_Load_Replay, Clock_Wait_Time: TOnClockTickEvent;
    end;

  TQueue = record
    Tail: Byte;
    Members: Array[1..QUEUE_MAX_PLAYERS] of Byte;
    RemainingSeconds: Byte;
    end;

  TChooseMap = record
    VoteInProgress: Boolean;
    MapChosen: Boolean;
    MapName: string;
    Vote_Num: Byte;
    Vote_Count: Byte;
    Time_Remaining: Byte;
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
  ChooseMap: TChooseMap;
  ReplayString: string;

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

procedure SetWaitingTime(Seconds: Integer);
begin
  Game.OnClockTick := Pointers.Clock_Wait_Time;
  RM.Countdown := MATH_SECOND_IN_TICKS * Seconds;
  RM.Active := True;
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
      if QueuePosResult = 1 then
        Queue.RemainingSeconds := QUEUE_TIMER;
      for I := QueuePosResult to Queue.Tail do
        if I < QUEUE_MAX_PLAYERS then
          Queue.Members[I] := Queue.Members[I + 1]
        else
          Queue.Members[I] := 0;
      Queue.Tail := Queue.Tail - 1;
      Result := True;
      Players.WriteConsole('[RM] ' + Players[MemberID].Name + ' has been removed from the queue.', MESSAGE_COLOR_GAME);
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
    Queue.RemainingSeconds := QUEUE_TIMER;
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
    if Queue.Tail = 0 then
      Queue.RemainingSeconds := QUEUE_TIMER;
    Queue.Tail := Queue.Tail + 1;
    Queue.Members[Queue.Tail] := NewMember;
  end else
    Result := False;
end;

function ReQueue(): Boolean;
begin
  Result := EnQueue(DeQueue());
end;

procedure QueueDisplayTurn();
begin
  if not IsQueueEmpty() then
  begin
    Players.WriteConsole('[RM] It is now ' + Players[Queue.Members[1]].Name + '''s turn.', MESSAGE_COLOR_GAME);
    Queue.RemainingSeconds := QUEUE_TIMER;
  end;
end;

procedure DoQueueUpdateOnTick();
begin
  if not IsQueueEmpty then
  begin
    if Queue.RemainingSeconds > 0 then
    begin
      Players.BigText(1, IntToStr(Queue.RemainingSeconds) + 's left for ' + Players[Queue.Members[1]].Name + '!',
        MATH_SECOND_IN_TICKS * 2, iif(Queue.RemainingSeconds > QUEUE_COLOR_YELLOW, MESSAGE_COLOR_GREEN,
        iif(Queue.RemainingSeconds > QUEUE_COLOR_RED, MESSAGE_COLOR_YELLOW, MESSAGE_COLOR_RED)),
        0.068, 5, 360);
      Queue.RemainingSeconds := Queue.RemainingSeconds - 1;
    end else
    begin
      Players.WriteConsole('[RM] ' + Players[Queue.Members[1]].Name + ' took too long and was removed from the queue.', MESSAGE_COLOR_GAME);
      DeQueue;
    end;
  end;
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
          Result[Length].KeyMask := DB_GetLong(DB_ID_REPLAYS,  0); // `Keys`
          Result[Length].AimX    := DB_GetLong(DB_ID_REPLAYS,  1); // `AimX`
          Result[Length].AimY    := DB_GetLong(DB_ID_REPLAYS,  2); // `AimY`
          Result[Length].PosX    := DB_GetFloat(DB_ID_REPLAYS, 3); // `PosX`
          Result[Length].PosY    := DB_GetFloat(DB_ID_REPLAYS, 4); // `PosY`
          Result[Length].VelX    := DB_GetFloat(DB_ID_REPLAYS, 5); // `VelX`
          Result[Length].VelY    := DB_GetFloat(DB_ID_REPLAYS, 6); // `VelY`
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
      Result         := DB_GetLong(DB_ID, 0);   // `rm_mapstats`.`mapID`
      RM.BestRunName := DB_GetString(DB_ID, 2); // `playerstats`.`name`
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

function Save_RunData(p: TActivePlayer; RunnerTime: TDateTime): Integer;
var
  ExistingRunID: Integer;
  PlayerID: Integer;
  DataBaseTime: TDateTime;
  GoldPlayer, SilverPlayer, BronzePlayer: Integer;
  PlayerNewRank: Integer;
  HWID: String;
begin
  Result := 0;
  if DB_CONNECTED then
  begin
    HWID := p.HWID;
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
          Achievement_Handle_Update(2, 1, p, True); // I like maps
        end;
      end;

      DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(PlayerID), IntToStr(DB_SERVER_ID),
        IntToStr(ACTION_KIND_RUN), Game.CurrentMap, FormatDateTime('nn:ss.zzz', RunnerTime)));

      DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val1(SQL_INC_RUNSNUM, IntToStr(RM.Map.MapID)));
      DB_FinishQuery(DB_ID);

      // Now check for any difference in medals rankings
      if Result > 0 then // Result is over 0 if something changed
      begin
        PlayerNewRank := GetPlayerRank(PlayerID, RM.Map.MapID);

        DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(PlayerID),
          IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_NEW_BEST), Game.CurrentMap, IntToStr(PlayerNewRank)));
        if (PlayerNewRank > 0) and (PlayerNewRank < 4) then
          Achievement_Handle_Update(3, 1, p, True); // Victory!
        case PlayerNewRank of
          MEDAL_GOLD:
          begin
            if PlayerID <> GoldPlayer then
              NewGoldMedal(PlayerID, GoldPlayer, SilverPlayer, BronzePlayer);
            Achievement_Handle_Update(6, 1, p, True); // That gold is mine!
            DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(PlayerID),
              IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_GOLD), Game.CurrentMap, 'gold'));
            if GoldPlayer > 0 then
              DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(GoldPlayer),
                IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_L_GOLD), Game.CurrentMap, 'gold'));
          end;
          MEDAL_SILVER:
          begin
            if PlayerID <> SilverPlayer then
              NewSilverMedal(PlayerID, SilverPlayer, BronzePlayer);
            Achievement_Handle_Update(5, 1, p, True); // 2nd place is the first loser
            DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(PlayerID),
              IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_SILVER), Game.CurrentMap, 'silver'));
            if SilverPlayer > 0 then
              DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(SilverPlayer),
                IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_L_SILVER), Game.CurrentMap, 'silver'));
          end;
          MEDAL_BRONZE:
          begin
            if PlayerID <> BronzePlayer then
              NewBronzeMedal(PlayerID, BronzePlayer);
            Achievement_Handle_Update(4, 1, p, True); // Bronzification
            DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(PlayerID),
              IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_BRONZE), Game.CurrentMap, 'bronze'));
            if BronzePlayer > 0 then
              DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(BronzePlayer),
                IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_L_BRONZE), Game.CurrentMap, 'bronze'));
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
begin
  Result := True;

  if DB_Open(DB_ID_REPLAYS, DB_CON_STRING_REPLAY, '', '', DB_Plugin_ODBC) <> 0 then
  begin
    WriteLn('[RM] Deleting replay if exists...');
    runIDString := IntToStr(runID);
    DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', DB_Query_Replace_Val2(SQL_DELETE_REPLAY, Game.CurrentMap, runIDString));
    WriteLn('[RM] Inserting new replay data...');

    DB_FinishQuery(DB_ID_REPLAYS);

    DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', 'SET @RUNID := ' + runIDString + ';');
    DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', ReplayString);

    DB_FinishQuery(DB_ID_REPLAYS);

    // Save CheckPoint data
    DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', DB_Query_Replace_Val2(SQL_DELETE_BESTRUN, Game.CurrentMap, runIDString));
    for I := 0 to RM.Map.AmountOfLaps - 1 do
      for J := 0 to RM.Map.AmountOfCheckPoints - 1 do
        DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', DB_Query_Replace_Val5(SQL_ADD_BESTRUN, Game.CurrentMap,
          runIDString, IntToStr(I + 1), IntToStr(J + 1), FormatDateTime('hh:nn:ss.zzz', RM.CurrentRunLap[I].Checkpoint[J])));

    DB_FinishQuery(DB_ID_REPLAYS);
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
  RM.TimeLeft := StrToDateTime(STR_TIME_LIMIT);
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
          WriteLnAndConsole(NIL, '[RM] The Map ' + MapToLoad + ' was loaded successfully!', MESSAGE_COLOR_GAME)
        else
          WriteLnAndConsole(NIL, '[RM] The Map ' + MapToLoad + ' could not been loaded!', MESSAGE_COLOR_RED);

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
  if not IsQueueEmpty() then
    if QueuePosition(RM.Runner.PPlayer.ID) = 1 then
      ReQueue()
    else
      if ReplayBot <> NIL then
        if RM.Runner.PPlayer.ID = ReplayBot.ID then
          ReQueue();
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

    WriteLnAndConsole(NIL, '[RM] ' + RM.Runner.PPlayer.Name + ' has finished a run in ' + FormatDateTime('nn:ss.zzz', RunTime), MESSAGE_COLOR_GAME);
    if ReplayBot <> NIL then
      if RM.Runner.PPlayer.ID <> ReplayBot.ID then
      begin
        WriteLnAndConsole(NIL, '[RM] Saving ' + RM.Runner.PPlayer.Name + '''s data.. This may take several seconds...', MESSAGE_COLOR_GAME);
        SetWaitingTime(MATH_SECOND * 3);

        Result_Run_ID := Save_RunData(RM.Runner.PPlayer, RunTime);
        if Result_Run_ID > 0 then
        begin
          if Save_ReplayData(Result_Run_ID, ReplayValues) then
            WriteLn('[RM] Saved the replay!')
          else
            WriteLn('[RM] Failed to save the replay!');
          LoadBestRun(GetBestRunIDOnMap(RM.Map.MapID));
        end;

        Achievement_Handle_Update(1, 1, RM.Runner.PPlayer, True); // Getting Started
      end else
        for j := 1 to HighID do
          if Players[j].Active then
            if Players[j].Human then
              Achievement_Handle_Update(71, 1, Players[j], False); // Magnificient
  end else
  begin
    WriteLnAndConsole(NIL, '[RM] ' + RM.Runner.PPlayer.Name + ' stopped his run.', MESSAGE_COLOR_GAME);
    DB_PerformConnectedQuery('EndSingleGame', DB_Query_Replace_Val1(SQL_INC_FAILSNUM, IntToStr(RM.Map.MapID)));
    DB_PerformConnectedQuery('EndSingleGame', DB_Query_Replace_Val5(SQL_INSERT_ACTION,
      IntToStr(DB_PlayerGetIDbyHWID(RM.Runner.PPlayer.HWID)), IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_FAIL),
      Game.CurrentMap, ''));
    SetWaitingTime(MATH_SECOND * 1);
    DB_FinishQuery(DB_ID);
  end;
  for j := 1 to 60 do
    RM.Runner.LastDistances[j] := 0;
  RM.Runner.PPlayer := NIL;

  if ChooseMap.MapChosen then
  begin
    Map.SetMap(ChooseMap.MapName);
    ChooseMap.MapChosen := False;
    ChooseMap.MapName   := '';
    SetWaitingTime(MATH_SECOND * 6);
  end;
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
  ReplayBot.KeyUp         := ReplayValues[CurrentLoop].KeyMask and BINARY_1  = BINARY_1;
  ReplayBot.KeyLeft       := ReplayValues[CurrentLoop].KeyMask and BINARY_2  = BINARY_2;
  ReplayBot.KeyRight      := ReplayValues[CurrentLoop].KeyMask and BINARY_3  = BINARY_3;
  ReplayBot.KeyJetpack    := ReplayValues[CurrentLoop].KeyMask and BINARY_4  = BINARY_4;
  ReplayBot.KeyGrenade    := ReplayValues[CurrentLoop].KeyMask and BINARY_5  = BINARY_5;
  ReplayBot.KeyChangeWeap := ReplayValues[CurrentLoop].KeyMask and BINARY_6  = BINARY_6;
  ReplayBot.KeyThrow      := ReplayValues[CurrentLoop].KeyMask and BINARY_7  = BINARY_7;
  ReplayBot.KeyCrouch     := ReplayValues[CurrentLoop].KeyMask and BINARY_8  = BINARY_8;
  ReplayBot.KeyProne      := ReplayValues[CurrentLoop].KeyMask and BINARY_9  = BINARY_9;
  ReplayBot.KeyShoot      := ReplayValues[CurrentLoop].KeyMask and BINARY_10 = BINARY_10;
  ReplayBot.MouseAimX     := ReplayValues[CurrentLoop].AimX;
  ReplayBot.MouseAimY     := ReplayValues[CurrentLoop].AimY;

  ReplayBot.Move(ReplayValues[CurrentLoop].PosX, ReplayValues[CurrentLoop].PosY);
  ReplayBot.SetVelocity(ReplayValues[CurrentLoop].VelX, ReplayValues[CurrentLoop].VelY);
end;

procedure RecordKeys();
var
  Len: Integer;
  KeyMask: Word;
begin
  Len := GetArrayLength(ReplayValues);

  KeyMask := 0;
  if RM.Runner.PPlayer.KeyUp then
    KeyMask := KeyMask or BINARY_1;
  if RM.Runner.PPlayer.KeyLeft then
    KeyMask := KeyMask or BINARY_2;
  if RM.Runner.PPlayer.KeyRight then
    KeyMask := KeyMask or BINARY_3;
  if RM.Runner.PPlayer.KeyJetpack then
    KeyMask := KeyMask or BINARY_4;
  if RM.Runner.PPlayer.KeyGrenade then
    KeyMask := KeyMask or BINARY_5;
  if RM.Runner.PPlayer.KeyChangeWeap then
    KeyMask := KeyMask or BINARY_6;
  if RM.Runner.PPlayer.KeyThrow then
    KeyMask := KeyMask or BINARY_7;
  if RM.Runner.PPlayer.KeyCrouch then
    KeyMask := KeyMask or BINARY_8;
  if RM.Runner.PPlayer.KeyProne then
    KeyMask := KeyMask or BINARY_9;
  if RM.Runner.PPlayer.KeyShoot then
    KeyMask := KeyMask or BINARY_10;

  if Len = 0 then
  begin
    ReplayString := 'INSERT INTO `' + Game.CurrentMap +
    '` (`replayOrder`, `runID`, `Keys`, `AimX`, `AimY`, `PosX`, `PosY`, `VelX`, `VelY`) VALUES' + FILE_NEWLINE;
    ReplayString := ReplayString + FILE_NEWLINE + '(' + IntToStr(Len) + ', ' + '@RUNID' + ', ' +
                                                  IntToStr(KeyMask) + ', ' +
                                                  IntToStr(RM.Runner.PPlayer.MouseAimX) + ', ' +
                                                  IntToStr(RM.Runner.PPlayer.MouseAimY) + ', ' +
                                                  FloatToStr(RM.Runner.PPlayer.X) + ', ' +
                                                  FloatToStr(RM.Runner.PPlayer.Y) + ', ' +
                                                  FloatToStr(RM.Runner.PPlayer.VelX) + ', ' +
                                                  FloatToStr(RM.Runner.PPlayer.VelY) + ')';
  end else
    ReplayString := ReplayString + ', ' + FILE_NEWLINE + '(' + IntToStr(Len) + ', ' + '@RUNID' + ', ' +
                                                         IntToStr(KeyMask) + ', ' +
                                                         IntToStr(RM.Runner.PPlayer.MouseAimX) + ', ' +
                                                         IntToStr(RM.Runner.PPlayer.MouseAimY) + ', ' +
                                                         FloatToStr(RM.Runner.PPlayer.X) + ', ' +
                                                         FloatToStr(RM.Runner.PPlayer.Y) + ', ' +
                                                         FloatToStr(RM.Runner.PPlayer.VelX) + ', ' +
                                                         FloatToStr(RM.Runner.PPlayer.VelY) + ')';
  SetArrayLength(ReplayValues, Len + 1);
end;

procedure PassingCheckPoints();
var
i,j: byte;
Distances: Single;
RunTime: TDateTime;
begin
  if RM.Runner.PPlayer.KeyReload then
  begin
    EndSingleGame(False);
    Exit;
  end;

  for i := 0 to RM.Map.AmountOfCheckpoints-1 do
    if not RM.Map.CheckPoints[i].Checked then
    begin
      Distances := Distance(RM.Map.CheckPoints[i].X,RM.Map.CheckPoints[i].Y,RM.Runner.PPlayer.X,RM.Runner.PPlayer.Y);
      break;
    end;

  RunTime := Now - RM.Runner.StartTime;
  if RM.BestRunLoaded and not BotActive then
    if RunTime - RM.BestRunLap[RM.Runner.Laps].CheckPoint[i] > StrToDateTime('00:00:20.000') then
    begin
      WriteLnAndConsole(NIL, '[RM] The runner ''' + RM.Runner.PPlayer.Name + ''' takes too long... Stopping his run!', MESSAGE_COLOR_RED);
      EndSingleGame(False);
    end;

  if Distances < RM.Map.CheckPoints[i].Distance then
  begin
    if i < RM.Map.AmountOfCheckpoints-1 then
    begin
      RM.Map.CheckPoints[i].Checked := True;
      RM.Runner.CP := i + 1;
      Players.WorldText(0, FormatDateTime('nn:ss.zzz', RunTime), MATH_SECOND_IN_TICKS * 2,
        MESSAGE_COLOR_GAME, 0.068, RM.Map.CheckPoints[i].X - 65, RM.Map.CheckPoints[i].Y + 50);
      RM.CurrentRunLap[RM.Runner.Laps].CheckPoint[i] := RunTime;
      if RM.BestRunLoaded and not BotActive then
        if RunTime > RM.BestRunLap[RM.Runner.Laps].CheckPoint[i] then
          Players.WorldText(1, '+' + FormatDateTime('nn:ss.zzz', RunTime - RM.BestRunLap[RM.Runner.Laps].CheckPoint[i]), MATH_SECOND_IN_TICKS * 2,
            MESSAGE_COLOR_RED,   0.068, RM.Map.CheckPoints[i].X - 85, RM.Map.CheckPoints[i].Y + 70)
        else
          Players.WorldText(1, '-' + FormatDateTime('nn:ss.zzz', RM.BestRunLap[RM.Runner.Laps].CheckPoint[i] - RunTime), MATH_SECOND_IN_TICKS * 2,
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
      RM.Runner.CP := 0;
      RM.Map.CheckPoints[RM.Map.AmountOfCheckpoints-1].Checked := True;
      DrawCheckPoints;
      RM.CurrentRunLap[RM.Runner.Laps - 1].CheckPoint[i] := RunTime;
      if RM.Runner.Laps = RM.Map.AmountOfLaps then
        EndSingleGame(True)
      else
      begin
        Players.WorldText(0, FormatDateTime('nn:ss.zzz', RunTime), MATH_SECOND_IN_TICKS * 2,
          MESSAGE_COLOR_GAME, 0.068, RM.Map.CheckPoints[i].X - 65, RM.Map.CheckPoints[i].Y + 50);
        if RM.BestRunLoaded and not BotActive then
          if RunTime > RM.BestRunLap[RM.Runner.Laps - 1].CheckPoint[i] then
            Players.WorldText(1, '+' + FormatDateTime('nn:ss.zzz', RunTime - RM.BestRunLap[RM.Runner.Laps - 1].CheckPoint[i]), MATH_SECOND_IN_TICKS * 2,
              MESSAGE_COLOR_RED,   0.068, RM.Map.CheckPoints[i].X - 85, RM.Map.CheckPoints[i].Y + 70)
          else
            Players.WorldText(1, '-' + FormatDateTime('nn:ss.zzz', RM.BestRunLap[RM.Runner.Laps - 1].CheckPoint[i] - RunTime), MATH_SECOND_IN_TICKS * 2,
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

procedure StartChooseMap(ChosenMap: string);
begin
  if ChooseMap.MapChosen then
  begin
    Players.WriteConsole('[RM] Next map has already been chosen: ' + ChooseMap.MapName, MESSAGE_COLOR_GAME);
    Exit;
  end;

  if ChooseMap.VoteInProgress then
  begin
    Players.WriteConsole('[RM] There is already a choosemap vote in progress!', MESSAGE_COLOR_GAME);
    Exit;
  end;

  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, DB_Escape_String(ChosenMap))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      ChooseMap.Vote_Num       := 0;
      ChooseMap.Vote_Count     := Game.NumPlayers - Game.NumBots;
      ChooseMap.VoteInProgress := True;
      ChooseMap.Time_Remaining := VOTE_TIME;
      Game.StartVoteMap(ChosenMap);
    end else
      Players.WriteConsole('[RM] Could not find the map ''' + ChosenMap + '''!', MESSAGE_COLOR_RED);
  end else
    WriteLn('[RM] Could not find the map for choosemap! Database is not connected!');
  DB_FinishQuery(DB_ID);
end;

function DenyVoteMap(p: TActivePlayer; VotedMap: string): Boolean;
begin
  p.WriteConsole('[RM] You cannot start a vote like that! Please see !commands', MESSAGE_COLOR_RED);
  Result := True;
end;

function DenyVoteKick(p, Victim: TActivePlayer; Reason: string): Boolean;
begin
  p.WriteConsole('[RM] You cannot start a vote like that! Please see !commands', MESSAGE_COLOR_RED);
  WriteLn('[RM] ' + p.Name + ' tried to start a votekick against ' + Victim.Name + '! Reason: ' + Reason);
  Result := True;
end;

procedure CountChooseMapVote(p: TActivePlayer; VotedMap: string);
var
  VoteSumPercent: Single;
begin
  ChooseMap.Vote_Num := ChooseMap.Vote_Num + 1;
  VoteSumPercent := ((1.0*ChooseMap.Vote_Num) / (1.0*ChooseMap.Vote_Count)) * 100.0;
  if VoteSumPercent >= VOTE_PERCENT then
  begin
    ChooseMap.VoteInProgress := False;
    WriteLnAndConsole(NIL, '[RM] The choosemap vote for map ' + VotedMap + ' was successful!', MESSAGE_COLOR_GAME);
    if not RM.Active then
    begin
      Map.SetMap(VotedMap);
      SetWaitingTime(MATH_SECOND * 6);
    end else
    begin
      ChooseMap.MapChosen      := True;
      ChooseMap.MapName        := VotedMap;
      Players.WriteConsole('[RM] Waiting for the current run to finish!', MESSAGE_COLOR_GAME);
    end;
  end else
    Players.WriteConsole('[RM] ' + FormatFloat('0.00', VoteSumPercent) + '% of ' +
      FormatFloat('0.00', VOTE_PERCENT) + '% for map ' + VotedMap + '!', MESSAGE_COLOR_GAME);
end;

procedure ShowTop(p: TActivePlayer; TypedCommand: String);
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
          p.WriteConsole('[RM] Could not find the map ''' + Text_Piece.Strings[1] + '''!', MESSAGE_COLOR_RED);
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
            p.WriteConsole('+-------------------------------------------------------------------------+', MESSAGE_COLOR_GAME);
            p.WriteConsole('| Showing ' + IntToStr(Top_X) +' of ' + TotalRuns + ' recorded runs on map ''' +
              SearchedMap + '''! ' + WHITESPACES[23 - Length(SearchedMap)] + WHITESPACES[23 - Length(IntToStr(Top_X))] +
              WHITESPACES[23 - Length(TotalRuns)] +'                |', MESSAGE_COLOR_GOLD);
            p.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);
            p.WriteConsole('| Rank | Name                     | Time (H:M:S.ms) | Date (Y-M-D H:M:S)  |', MESSAGE_COLOR_GAME);
            p.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);
          end;
          PlayerName := DB_GetString(DB_ID, 2);
          p.WriteConsole('| #' + IntToStr(RankID) + WHITESPACES[20 + Length(IntToStr(RankID))] + ' | ' + PlayerName + WHITESPACES[Length(PlayerName) - 1] + ' | ' +
            DB_GetString(DB_ID, 3) + 's   | ' + DB_GetString(DB_ID, 4) + ' | [' + DB_GetString(DB_ID, 0) + ']', Medal_Color_by_Rank(RankID));
          RankID := RankID + 1;
        end;
        p.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);

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
    // Text_Piece.Strings[1] can be a map or player or achievement
    // Text_Piece.Strings[2] is the string you are searching for
    SplitRegExpr(' ', Text, Text_Piece);

    if Text_Piece.Count < 3 then
    begin
      p.WriteConsole('[RM] The command you have typed was incomplete (!search ''map''/''player''/''achievement'' <name>)!', MESSAGE_COLOR_RED);
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
          if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_MAP_BY_N, DB_Escape_String_Select(Text_Piece.Strings[2]))) <> 0) AND
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
          if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_PLR_BY_N, DB_Escape_String_Select(Text_Piece.Strings[2]))) <> 0) AND
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
        'achievement':
        begin
          if Length(Text_Piece.Strings[2]) > 16 then
          begin
            p.WriteConsole('[RM] The achievement name you was searching for is too long!', MESSAGE_COLOR_RED);
            Exit;
          end;

          p.WriteConsole('+------+------------------------------------------------------------+', MESSAGE_COLOR_GAME);
          p.WriteConsole('| ID   | Search achievement: ' + Text_Piece.Strings[2] + WHITESPACES[7 + Length(Text_Piece.Strings[2])] +
                          '                       |', MESSAGE_COLOR_GAME);
          p.WriteConsole('+------+------------------------------------------------------------+', MESSAGE_COLOR_GAME);
          if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_ACH_BY_N, DB_Escape_String_Select(Text_Piece.Strings[2]))) <> 0) AND
             (DB_NextRow(DB_ID) <> 0) then
          begin
            ResultString := DB_GetString(DB_ID, 0);  // `mapname`
            ResID        := DB_GetString(DB_ID, 1);  // `ID`
            while Length(ResultString) < 58 do
              ResultString := ResultString + ' ';
            while Length(ResID) < 4 do
              ResID := ResID + ' ';
            p.WriteConsole('| ' + ResID + ' | ' + ResultString + ' |', MESSAGE_COLOR_GAME);
            while DB_NextRow(DB_ID) <> 0 do
            begin
              ResultString := DB_GetString(DB_ID, 0);  // `mapname`
              ResID        := DB_GetString(DB_ID, 1);  // `ID`
              while Length(ResultString) < 58 do
                ResultString := ResultString + ' ';
              while Length(ResID) < 4 do
                ResID := ResID + ' ';
              p.WriteConsole('| ' + ResID + ' | ' + ResultString + ' |', MESSAGE_COLOR_GAME);
            end;
            p.WriteConsole('+------+------------------------------------------------------------+', MESSAGE_COLOR_GAME);
            DB_FinishQuery(DB_ID);
          end else
          begin
            p.WriteConsole('| ---- | No achievement was found.                                  |', MESSAGE_COLOR_GAME);
            p.WriteConsole('+------+------------------------------------------------------------+', MESSAGE_COLOR_GAME);
            DB_FinishQuery(DB_ID);
          end;
        end;
        else
          p.WriteConsole('[RM] Please specify if you search a map or player! (!search ''map''/''player''/''achievement'' <name>)', MESSAGE_COLOR_RED);
      end;
    end else
      WriteLnAndConsole(p, '[RM] Could not perform a search! Database is not connected!', MESSAGE_COLOR_SYSTEM);
  except
    WriteLn('[RM] Some error happened in PerformSearch! Cannot figure out what...');
  finally
    Text_Piece.Free;
  end;
end;

function LastActionSentence(Kind: Byte; Info, Info2, Name: string): string;
begin
  case Kind of
    ACTION_KIND_RUN:      Result := Name + ' finished a run on ' + Info + ' in ' + Info2;
    ACTION_KIND_FAIL:     Result := Name + ' stopped a run on ' + Info;
    ACTION_KIND_VS_WON:   Result := Name + ' won a vs race on ' + Info;
    ACTION_KIND_VS_LOST:  Result := Name + ' lost a vs race on ' + Info;
    ACTION_KIND_ACHIEVE:
    begin
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_ACHIEVE_INFO_1, DB_Escape_String(Info))) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
        Result := Name + ' earned the achievement ' + DB_GetString(DB_ID, 0) + ' (' + DB_GetString(DB_ID, 1) + ' points)'
      else
        Result := 'An error happened while displaying AchievementID ' + Info + '!';
      DB_FinishQuery(DB_ID);
    end;
    ACTION_KIND_GOLD,
    ACTION_KIND_SILVER,
    ACTION_KIND_BRONZE:   Result := Name + ' got a ' + Info2 + ' medal on ' + Info;
    ACTION_KIND_L_GOLD,
    ACTION_KIND_L_SILVER,
    ACTION_KIND_L_BRONZE: Result := Name + ' lost a ' + Info2 + ' medal on ' + Info;
    ACTION_KIND_NEW_BEST: Result := Name + ' has a new best (Rank: ' + Info2 + ') on ' + Info;
    ACTION_KIND_JOIN:     Result := Name + ' has joined the game';
    ACTION_KIND_LEAVE:    Result := Name + ' has left the game';
  else
    Result := 'ERROR - DEBUG DATA ' + IntToStr(Kind) + ' ' + Info + ' ' + Name;
  end;
end;

procedure ShowLast15(p: TActivePlayer);
begin
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_RECENT_ACTIONS15, IntToStr(DB_PlayerGetIDbyHWID(p.HWID)))) <> 0) THEN
    begin
      p.WriteConsole('[RM] Your last 15 actions:', MESSAGE_COLOR_GAME);
      // `serverID` = 0, `time` = 1, `Kind` = 2, `info` = 3, `info2` = 4
      while DB_NextRow(DB_ID) <> 0 do
        p.WriteConsole('[' + DB_GetString(DB_ID, 1) + '] ' + LastActionSentence(DB_GetLong(DB_ID, 2),
          DB_GetString(DB_ID, 3), DB_GetString(DB_ID, 4), p.Name), MESSAGE_COLOR_GAME);
      p.WriteConsole('[RM] That''s all.', MESSAGE_COLOR_GAME);
    end else
    begin
      WriteLnAndConsole(p, '[RM] Could not find any recent actions from you!', MESSAGE_COLOR_SYSTEM);
    end;
    DB_FinishQuery(DB_ID);
  end else
    WriteLnAndConsole(p, '[RM] Could not show the statistics! Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure ShowProfile(p: TActivePlayer; ProfileIDText: string);
var
  ProfileID: Integer;
  ProfileName: string;
  ProfileGolds: string;
  ProfileSilvers: string;
  ProfileBronzes: string;
  ProfileFirstJoin: string;
  ProfileLastJoin: string;
  ProfileAchievementNum: string;
  ProfileAchievementPoints: string;
  ProfileRecentAchievements: string;
  ProfileRecentAction: string;
begin
  try
    ProfileID := StrToInt(ProfileIDText);
  except
    p.WriteConsole('[RM] Could not load profile ''' + ProfileIDText + '''! It is not a numeric value!', MESSAGE_COLOR_RED);
    Exit;
  end;
  if ProfileID <= 0 then
  begin
    p.WriteConsole('[RM] Could not load profile ''' + ProfileIDText + '''! The ID needs to be 1 or higher!', MESSAGE_COLOR_RED);
    Exit;
  end;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_PLR_BY_ID, ProfileIDText)) <> 0) AND
       (DB_NextRow(DB_ID) <> 0) then
    begin
      ProfileName      := DB_GetString(DB_ID, 1); // `name`
      ProfileGolds     := DB_GetString(DB_ID, 2); // `gold`
      ProfileSilvers   := DB_GetString(DB_ID, 3); // `silver`
      ProfileBronzes   := DB_GetString(DB_ID, 4); // `bronze`
      ProfileFirstJoin := DB_GetString(DB_ID, 5); // `firstjoin`
      ProfileLastJoin  := DB_GetString(DB_ID, 6); // `lastseen`
      p.WriteConsole('+--------------------------------+----------------------------------------------------------------+', MESSAGE_COLOR_GAME);
      p.WriteConsole('| Name: ' + ProfileName + WHITESPACES[Length(ProfileName) - 1] + ' | First/Last seen: ' + ProfileFirstJoin + ' / ' + ProfileLastJoin +
                      '     |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+--------------------------------+------------------+--------------------+------------------------+', MESSAGE_COLOR_GAME);
      p.WriteConsole('|             Medals             | Golds: ' + ProfileGolds + WHITESPACES[14 + Length(ProfileGolds)] + ' | Silvers: ' + ProfileSilvers
                      + WHITESPACES[14 + Length(ProfileSilvers)] + ' | Bronzes: ' + ProfileBronzes + WHITESPACES[14 + Length(ProfileBronzes)] +
                      '     |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+--------------------------------+------------------+----------+---------+------------------------+', MESSAGE_COLOR_GAME);
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_PLAYER_ACHIEVES, ProfileIDText)) <> 0) AND
         (DB_NextRow(DB_ID) <> 0) then
      begin
        ProfileAchievementPoints := DB_GetString(DB_ID, 0); // SUM(`rm_achievements`.`Points`)
        ProfileAchievementNum    := DB_GetString(DB_ID, 1); // COUNT(`rm_achievements`.`ID`)
        p.WriteConsole('|          Achievements          | Earned: ' + ProfileAchievementNum + WHITESPACES[14 + Length(ProfileAchievementNum)] +
                        '           | Points: ' + ProfileAchievementPoints + WHITESPACES[14 + Length(ProfileAchievementPoints)] +
                        '                |', MESSAGE_COLOR_GAME);
        p.WriteConsole('+--------------------------------+-----------------------------+----------------------------------+', MESSAGE_COLOR_GAME);
      end;
      p.WriteConsole('| Recent achievements                                                                             |' + FILE_NEWLINE +
                     '|                                                                                                 |', MESSAGE_COLOR_GAME);
      ProfileRecentAchievements := '';
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_ACHIEVE_RECENT, IntToStr(ProfileID))) <> 0) then
      begin
        // `rm_achievements`.`Name` = 0, `rm_achievements`.`Points` = 1, `rm_achievements_claim`.`ClaimDate` = 2
        while DB_NextRow(DB_ID) <> 0 do
          ProfileRecentAchievements := ProfileRecentAchievements + '  [' + DB_GetString(DB_ID, 2) + '] ' +
            DB_GetString(DB_ID, 0) + ' (Points: ' + DB_GetString(DB_ID, 1) + ')' + FILE_NEWLINE;
      end;
      if ProfileRecentAchievements = '' then
        p.WriteConsole('  Could not find any recent achievements!', MESSAGE_COLOR_SYSTEM)
      else
        p.WriteConsole(ProfileRecentAchievements, MESSAGE_COLOR_GAME);
      p.WriteConsole('|                                                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('|                                                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('|                                                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('|                                                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+-------------------------------------------------------------------------------------------------+', MESSAGE_COLOR_GAME);
      p.WriteConsole('| Recent activity                                                                                 |' + FILE_NEWLINE +
                     '|                                                                                                 |', MESSAGE_COLOR_GAME);
      ProfileRecentAction := '';
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_RECENT_ACTIONS5, IntToStr(ProfileID))) <> 0) then
      begin
        // `serverID` = 0, `time` = 1, `Kind` = 2, `info` = 3, `info2` = 4
        while DB_NextRow(DB_ID) <> 0 do
          ProfileRecentAction := ProfileRecentAction + '  [' + DB_GetString(DB_ID, 1) + '] ' +
            LastActionSentence(DB_GetLong(DB_ID, 2), DB_GetString(DB_ID, 3), DB_GetString(DB_ID, 4), ProfileName) + FILE_NEWLINE;
      end;
      if ProfileRecentAction = '' then
        p.WriteConsole('  Could not find any recent actions!', MESSAGE_COLOR_SYSTEM)
      else
        p.WriteConsole(ProfileRecentAction, MESSAGE_COLOR_GAME);
      p.WriteConsole('|                                                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('|                                                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('|                                                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('|                                                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+-------------------------------------------------------------------------------------------------+', MESSAGE_COLOR_GAME);
    end else
      p.WriteConsole('[RM] Could not find the profile you were searching for!', MESSAGE_COLOR_RED);
  end else
    WriteLnAndConsole(p, '[RM] Could not search for a profile because Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure FindAndShowProfile(p: TActivePlayer; ProfileName: string);
begin
  if Length(ProfileName) = 0 then
    ShowProfile(p, IntToStr(DB_PlayerGetIDbyHWID(p.HWID)))
  else
    if DB_CONNECTED then
    begin
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_PLR_BY_N, DB_Escape_String(ProfileName))) <> 0) AND
         (DB_NextRow(DB_ID) <> 0) then
        ShowProfile(p, DB_GetString(DB_ID, 0)) // `ID`
      else
        p.WriteConsole('[RM] Could not find a profile for ''' + ProfileName + '''!', MESSAGE_COLOR_RED);
    end else
      WriteLnAndConsole(p, '[RM] Could not search for a profile because Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure ShowAchievement(p: TActivePlayer; AchievementIDText: string);
var
  AchievementID: Integer;
begin
  try
    AchievementID := StrToInt(AchievementIDText);
  except
    p.WriteConsole('[RM] Could not load achievement ''' + AchievementIDText + '''! It is not a numeric value!', MESSAGE_COLOR_RED);
    Exit;
  end;
  if AchievementID <= 0 then
  begin
    p.WriteConsole('[RM] Could not load achievement ''' + AchievementIDText + '''! The ID needs to be 1 or higher!', MESSAGE_COLOR_RED);
    Exit;
  end;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_ACH_BY_ID, AchievementIDText)) <> 0) AND
       (DB_NextRow(DB_ID) <> 0) then
    begin
      p.WriteConsole('[RM] Name:           ' + DB_GetString(DB_ID, 1), MESSAGE_COLOR_GAME); // `rm_achievements`.`Name`
      p.WriteConsole('[RM] Description:    ' + DB_GetString(DB_ID, 2), MESSAGE_COLOR_GAME); // `rm_achievements`.`Description`
      p.WriteConsole('[RM] Objective:      ' + DB_GetString(DB_ID, 3), MESSAGE_COLOR_GAME); // `rm_achievements`.``ObjectiveText`
      p.WriteConsole('[RM] Points:         ' + DB_GetString(DB_ID, 4), MESSAGE_COLOR_GAME); // `rm_achievements`.`Points`
      p.WriteConsole('[RM] First achiever: ' + DB_GetString(DB_ID, 5), MESSAGE_COLOR_GAME); // `playerstats`.`Name`
      p.WriteConsole('[RM] Do you have it? ' + iif(Achievement_Has_ID_Finished(AchievementID,
        DB_PlayerGetIDbyHWID(p.HWID)), 'Yes', 'No'), MESSAGE_COLOR_GAME); 
    end else
      p.WriteConsole('[RM] The achievement you searched for was not found!', MESSAGE_COLOR_RED);
  end else
    WriteLnAndConsole(p, '[RM] Could not search for a profile because Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure ShowAchievementList(p: TActivePlayer; AdditionalText: string);
var
  GetPage: Integer;
  PlayerID: Integer;
  OutPutLine: string;
  ResString: string;
  OutColor: Cardinal;
  BOX_LINE_LENGTH: Byte;
begin
  if Length(AdditionalText) > 0 then
  begin
    try
      GetPage := StrToInt(AdditionalText);
    except
      p.WriteConsole('[RM] Could not load achievement page ''' + AdditionalText + '''! It is not a numeric value!', MESSAGE_COLOR_RED);
      Exit;
    end;

    if GetPage <= 0 then
    begin
      p.WriteConsole('[RM] Could not load achievement page ''' + AdditionalText + '''! The Page needs to be 1 or higher!', MESSAGE_COLOR_RED);
      Exit;
    end;

    // Even tho we do not have that many achievement pages. Just being sure.
    if GetPage > 20 then
    begin
      p.WriteConsole('[RM] Could not load achievement page ''' + AdditionalText + '''! The Page needs to be maximal 20!', MESSAGE_COLOR_RED);
      Exit;
    end;
  end else
    GetPage := 1;

  if DB_CONNECTED then
  begin
    BOX_LINE_LENGTH := 77;
    PlayerID := DB_PlayerGetIDbyHWID(p.HWID);
    // 20 results per Page, that's why GetPage * 20
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_ACHIEVE_LIST, IntToStr(PlayerID), IntToStr((GetPage-1) * 20))) <> 0) then
    begin
      // `rm_achievements`.`ID` = 0, `rm_achievements`.`Name` = 1, `rm_achievements`.`Points` = 2,
      // `rm_achievements_claim`.`ClaimDate` = 3,
      // `rm_achievements_progress`.`Progress` = 4, `rm_achievements_progress`.`Requirement` = 5
      OutPutLine := 'Achievements List (Page ' + IntToStr(GetPage) + ')                                                 ';
      while Length(OutPutLine) < BOX_LINE_LENGTH do
        OutPutLine := OutPutLine + ' ';
      p.WriteConsole('+-------------------------------------------------------------------------------+', MESSAGE_COLOR_GAME);
      p.WriteConsole('| ' + OutPutLine + ' |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+------+-----------------------------------------------+--------+---------------+', MESSAGE_COLOR_GAME);
      p.WriteConsole('| ID   | Name                                          | Points | Progress      |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+------+-----------------------------------------------+--------+---------------+', MESSAGE_COLOR_GAME);
      OutPutLine := '';
      while DB_NextRow(DB_ID) <> 0 do
      begin
        ResString := DB_GetString(DB_ID, 0);
        while Length(ResString) < 4 do
          ResString := ResString + ' ';
        OutPutLine := ResString + ' |';

        ResString := DB_GetString(DB_ID, 1);
        while Length(ResString) < 45 do
          ResString := ResString + ' ';
        OutPutLine := OutPutLine + ' ' + ResString + ' |';

        ResString := DB_GetString(DB_ID, 2);
        while Length(ResString) < 6 do
          ResString := ResString + ' ';
        OutPutLine := OutPutLine + ' ' + ResString + ' |';

        ResString := DB_GetString(DB_ID, 3);
        // If has not yet finished
        if ResString = '' then
        begin
          ResString := DB_GetString(DB_ID, 4);
          // If has not in progress
          if ResString = '' then
          begin
            OutPutLine := OutPutLine + ' Not started  ';
            OutColor := MESSAGE_COLOR_GAME;
          end else
          begin
            OutPutLine := OutPutLine + ' ' + ResString + '/' + DB_GetString(DB_ID, 5);
            while Length(OutPutLine) < BOX_LINE_LENGTH do
              OutPutLine := OutPutLine + ' ';
            OutColor := MESSAGE_COLOR_YELLOW;
          end;
        end else
        begin
          OutPutLine := OutPutLine + ' Finished     ';
          OutColor := MESSAGE_COLOR_GREEN;
        end;

        p.WriteConsole('| ' + OutPutLine + ' |', OutColor);
      end;
      if OutPutLine = '' then
        p.WriteConsole('| This page seems to be quite empty. Maybe try a previous page.                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+------+-----------------------------------------------+--------+---------------+', MESSAGE_COLOR_GAME);
    end else
      WriteLnAndConsole(p, '[RM] Error: Could not Display any achievements (' + DB_Error() + ')!', MESSAGE_COLOR_SYSTEM);
  end else
    WriteLnAndConsole(p, '[RM] Could not search for a profile because Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure ShowQueue(p: TActivePlayer);
var
  I: Byte;
begin
  if IsQueueEmpty() then
    p.WriteConsole('[RM] The queue is empty!', MESSAGE_COLOR_GAME)
  else
  begin
    p.WriteConsole('+-----------------------------+', MESSAGE_COLOR_GAME);
    p.WriteConsole('| Players in queue            |', MESSAGE_COLOR_GAME);
    p.WriteConsole('+-----------------------------+', MESSAGE_COLOR_GAME);
    for I := 1 to Queue.Tail do
      p.WriteConsole('| ' + IntToStr(I) + '. ' + Players[Queue.Members[I]].Name +
        WHITESPACES[Length(Players[Queue.Members[I]].Name) - 1] + ' |', Medal_Color_by_Rank(I));
    p.WriteConsole('+-----------------------------+', MESSAGE_COLOR_GAME);
  end;
end;

procedure ShowStatistics(p: TActivePlayer);
begin
  if DB_CONNECTED then
  begin
    DB_FinishQuery(DB_ID);
    if (DB_Query(DB_ID, SQL_GET_STATISTICS) <> 0) AND
       (DB_NextRow(DB_ID) <> 0) then
    begin
      p.WriteConsole('+-------------------+---------------------------------------------+', MESSAGE_COLOR_GAME);
      p.WriteConsole('| Server statistics |        RunMode 3 created by ExHunter        |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+-------------------+---------------------------------------------+'     + #13#10 +
                     '|                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('  We have in total ' + DB_GetString(DB_ID, 0) + ' recorded runs.'        + #13#10 + // totalruns
                     '  Ran by ' + DB_GetString(DB_ID, 1) + ' different players.', MESSAGE_COLOR_GOLD);   // differentplayers
      p.WriteConsole('|                                                                 |'     + #13#10 +
                     '|                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('  On ' + DB_GetString(DB_ID, 3) + ' different maps.'                     + #13#10 + // totalmaps
                     '  There are in total ' + DB_GetString(DB_ID, 2) + ' achievement points.',           // totalachievementpoints
                     MESSAGE_COLOR_GOLD);
      p.WriteConsole('|                                                                 |'     + #13#10 +
                     '|                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('  In ' + DB_GetString(DB_ID, 4) + ' different achievements.'             + #13#10 + // totaldifferentachievements
                    '  There are already ' + DB_GetString(DB_ID, 6) + ' runs and ' + DB_GetString(DB_ID, 5) + ' fails made.',
                    MESSAGE_COLOR_GOLD);  // totaltries = 6 and totalfails = 5
      p.WriteConsole('|                                                                 |', MESSAGE_COLOR_GAME);
      p.WriteConsole('+-----------------------------------------------------------------+', MESSAGE_COLOR_GAME);
    end else
    begin
      WriteLnAndConsole(p, '[RM] Could not find the statistics!', MESSAGE_COLOR_SYSTEM);
      WriteLn('[RM] Error in ShowStatistics: ' + DB_Error());
    end;
    DB_FinishQuery(DB_ID);
  end else
    WriteLnAndConsole(p, '[RM] Could not show the statistics! Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure ShowMapsList(p: TActivePlayer);
var
  LineOutPut: string;
  MapName: string;
begin
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, SQL_GET_MAPSLIST) <> 0) then
    begin
      LineOutPut := '';
      MapName    := '';
      p.WriteConsole('+-----------------------------------------------------------------+', MESSAGE_COLOR_GAME);
      p.WriteConsole('| MapsList (a set of up to 75 random maps)                        |', MESSAGE_COLOR_GOLD);
      p.WriteConsole('+-----------------------------------------------------------------+', MESSAGE_COLOR_GAME);
      while DB_NextRow(DB_ID) <> 0 do
      begin
        MapName := DB_GetString(DB_ID, 0); // `mapname`
        if ((Length(LineOutPut + MapName) + 3) > 62) then
        begin
          while Length(LineOutPut) < 64 do
            LineOutPut := LineOutPut + ' ';
          p.WriteConsole('| ' + LineOutPut + '|', MESSAGE_COLOR_GAME);
          LineOutPut := '[' + MapName + '] ';
        end else
          LineOutPut := LineOutPut + '[' + MapName + '] ';
      end;
      if LineOutPut = '' then
        p.WriteConsole('| No map has been found.                                          |', MESSAGE_COLOR_GAME)
      else
      begin
        while Length(LineOutPut) < 64 do
          LineOutPut := LineOutPut + ' ';
        p.WriteConsole('| ' + LineOutPut + '|', MESSAGE_COLOR_GAME);
      end;
      p.WriteConsole('+-----------------------------------------------------------------+', MESSAGE_COLOR_GAME);
    end else
      WriteLnAndConsole(p, '[RM] Could not find the mapslist!', MESSAGE_COLOR_SYSTEM);
    DB_FinishQuery(DB_ID);
  end else
    WriteLnAndConsole(p, '[RM] Could not show the mapslist! Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure ShowRuns(p: TActivePlayer);
var
  I: Byte;
  RankID: Integer;
  PlayerName: string;
begin
  if DB_CONNECTED then
  begin
    p.WriteConsole('+-------------------------------------------------------------------------+', MESSAGE_COLOR_GAME);
    p.WriteConsole('| List of runs for online players on current map                          |', MESSAGE_COLOR_GOLD);
    p.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);
    p.WriteConsole('| Rank | Name                     | Time (H:M:S.ms) | Date (Y-M-D H:M:S)  |', MESSAGE_COLOR_GAME);
    p.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);
    PlayerName := '';
    for I := 1 to HighID do
      if Players[I].Human then
      begin
        if (DB_Query(DB_ID, SQL_GET_RANK_1_OF_2) <> 0) and
           (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_RANK_2_OF_2, IntToStr(RM.Map.MapID),
            IntToStr(DB_PlayerGetIDbyHWID(Players[I].HWID)))) <> 0) and
           (DB_NextRow(DB_ID) <> 0) then
        begin
          // `rank` = 0, `playerID` = 1, `runtime` = 2, `rundate` = 3, `ID` = 4
          RankID := DB_GetLong(DB_ID, 0); // `rank`
          PlayerName := Players[I].Name;
          p.WriteConsole('| #' + IntToStr(RankID) + WHITESPACES[20 + Length(IntToStr(RankID))] + ' | ' + PlayerName + WHITESPACES[Length(PlayerName) - 1] + ' | ' +
            DB_GetString(DB_ID, 2) + 's   | ' + DB_GetString(DB_ID, 3) + ' | [' + DB_GetString(DB_ID, 4) + ']', Medal_Color_by_Rank(RankID));
        end;
        DB_FinishQuery(DB_ID);
      end;
      if PlayerName = '' then
        p.WriteConsole('| Nobody online has made a run on this map.                               |', MESSAGE_COLOR_GAME);
    p.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);
  end else
    WriteLnAndConsole(p, '[RM] Could not show the runs! Database is not connected!', MESSAGE_COLOR_SYSTEM);
end;

procedure ShowBestRun(p: TActivePlayer);
var
  I, J: Byte;
begin
  if RM.BestRunLoaded then
  begin
    p.WriteConsole('+--------------------------+', MESSAGE_COLOR_GAME);
    p.WriteConsole('| ' + RM.BestRunName + WHITESPACES[Length(RM.BestRunName) - 1] + ' |', MESSAGE_COLOR_GOLD);
    p.WriteConsole('|                          |', MESSAGE_COLOR_GAME);
    for I := 0 to RM.Map.AmountOfLaps - 1 do
    begin
      p.WriteConsole('|   Lap ' + IntToStr(I + 1) + WHITESPACES[23 - Length(IntToStr(I))] + '    +------------+', MESSAGE_COLOR_GAME);
      for J := 0 to RM.Map.AmountOfCheckpoints - 1 do
        p.WriteConsole('| CP ' + IntToStr(J + 1) + WHITESPACES[23 - Length(IntToStr(J))] + '       | ' + FormatDateTime('nn:ss.zzz', RM.BestRunLap[I].CheckPoint[J]) + 's |', MESSAGE_COLOR_GAME);
    end;
    p.WriteConsole('+-------------+------------+', MESSAGE_COLOR_GAME);
  end else
    WriteLnAndConsole(p, '[RM] Could not load the best run for this map!', MESSAGE_COLOR_SYSTEM);
end;

procedure OnSpeak(p: TActivePlayer; Text: string);
begin
  if Text[1] = '!' then
    case LowerCase(ReplaceRegExpr(REGEXP_FIRST_WORD, Text, '', False)) of
      '!play',
      '!start':
      begin
        if not RM.Active and RM.Map.Loaded then
          if IsQueueEmpty() then
            p.Team := TEAM_RUNNER
          else
            if QueuePosition(p.ID) = 1 then
              p.Team := TEAM_RUNNER
            else
              p.WriteConsole('[RM] It''s not your turn yet! If you did not add yourself yet, do !add', MESSAGE_COLOR_RED);
      end;
      '!fail',
      '!stop',
      '!quit',
      '!giveup':
      begin
        if RM.Active then
          if RM.Runner.PPlayer <> NIL then
            if p.ID = RM.Runner.PPlayer.ID then
              EndSingleGame(False);
      end;
      '!freerun',
      '!delta':
      begin
        p.Team := TEAM_FREERUNNER;
        Achievement_Handle_Update(69, 1, p, False); // Keep on runnin'...
      end;
      '!add':
      begin
        if QueuePosition(p.ID) = 0 then
        begin
          EnQueue(p.ID);
          p.WriteConsole('[RM] Successfully added to the queue!', MESSAGE_COLOR_GAME);
          Achievement_Handle_Update(68, 1, p, False); // Queue.. cuisine? What?
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
      '!queue': ShowQueue(p);
      '!top': ShowTop(p, Text);
      '!top10': ShowTop(p, Text);
      '!admin':
      begin
        Players.WriteConsole('[RM] ' + p.Name + ' has requested an admin.', MESSAGE_COLOR_GREEN);
        Achievement_Handle_Update(70, 1, p, False); // Naggers
      end;
      '!adminlist',
      '!admins':
      begin
        p.WriteConsole('+-----A-D-M-I-N-S-----+', MESSAGE_COLOR_GAME);
        p.WriteConsole('| ExHunter       (EU) |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| Toyux          (SA) |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('+---------------------+', MESSAGE_COLOR_GAME);
        p.WriteConsole('| HaSte          (EU) |', MESSAGE_COLOR_GAME);
        p.WriteConsole('+---------------------+', MESSAGE_COLOR_GAME);
      end;
      '!rules':
      begin
        p.WriteConsole('+--------------D-A--R-U-L-E-S--------------+', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| 1. Do not use cheats or hacks            |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| 2. Do not abuse bugs                     |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| 3. Take turns                            |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| 4. Respect other players and admins      |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| 5. What an admin says is word            |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('|    You can argue, but do not stress it.  |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('|                                          |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| We reserve us  the  right  to ban you on |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| on any server  in  our  network,  if you |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| break any  rules  here.  Also we measure |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| on our own how  we  punish rulebreakers. |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| This can include:  warnings, bans, stats |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| being removed, mutes, etc.               |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('+------------------------------------------+', MESSAGE_COLOR_GOLD);
      end;
      '!profile': FindAndShowProfile(p, Copy(Text, 10, Length(Text) - 9));
      '!profileid': ShowProfile(p, Copy(Text, 12, Length(Text) - 11));
      '!achievements': ShowAchievementList(p, Copy(Text, 15, Length(Text) - 14));
      '!achievementid': ShowAchievement(p, Copy(Text, 16, Length(Text) - 15));
      '!commands',
      '!cmds':
      begin
        p.WriteConsole('+----------------R-U-N-M-O-D-E--C-O-M-M-A-N-D-S----------------+', MESSAGE_COLOR_GAME);
        p.WriteConsole('| COMMAND               | DESCRIPTION                          |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('+-----------------------+--------------------------------------+', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !play / !start        | start a run                          |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !freerun / !delta     | you can move freely around           |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !fail / !quit         |                                      |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !stop / !giveup       | stops a run                          |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !add                  | adds you to the queue                |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !del                  | removes you from the queue           |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !queue                | players in the queue                 |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !bestrun              | shows times of the best run          |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !search map/player ?  | searches for the map or player <?>   |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !search achievement ? | searches for the achievement <?>     |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !achievements ?       | achievementlist, <optional:?> = page |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !achievementid ?      | info about achievement ID <?>        |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !last15               | your last 15 actions                 |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !maps / !mapslist     | up to 75 maps from mapslist          |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !top !top10           | the top on current map               |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !top ? / !top10 ?     | top players on map <?>               |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !runs                 | list runs of online players          |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !help / !info         | a litte info about the game          |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !rules                | rules for this server                |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| !profile ?            | profile of player name <optional:?>  |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !profileid ?          | profile of ID <?> from search player |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !statistics           | some server statistics               |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !adminlist            | server admins                        |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !choosemap / !vote ?  | starts vote for map <?>              |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| !replay ?             | replay RunID <?> (in top [] number)  |', MESSAGE_COLOR_GAME);
        p.WriteConsole('+-----------------------+--------------------------------------+', MESSAGE_COLOR_GAME);
      end;
      '!last15': ShowLast15(p);
      '!maps',
      '!mapslist': ShowMapsList(p);
      '!runs': ShowRuns(p);
      '!help',
      '!info':
      begin
        p.WriteConsole('+-----------R-U-N-M-O-D-E--I-N-F-O-----------+', MESSAGE_COLOR_GAME);
        p.WriteConsole('| Script by ExHunter, original idea by Vince |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('+--------------------------------------------+', MESSAGE_COLOR_GAME);
        p.WriteConsole('| This is a RunMode3 server. Try to find out |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| how   fast   you   are!   Just   pass  the |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| checkpoints  (numbers)  while  playing and |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| brag around your friends with the results! |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| Compete solo  or  in  a  versus mode, hunt |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| for achievements and medals! Be one of the |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| fastest players in Soldat!                 |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| Or you know... Just run here for fun!      |', MESSAGE_COLOR_GAME);
        p.WriteConsole('+--------------------------------------------+', MESSAGE_COLOR_GAME);
        p.WriteConsole('| Instructions for your first run:           |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('| Type !play while  nobody  else is playing, |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| then start to run  to  the next red number |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| (starting with 1).  If you pass the number |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| it will appear now as green, so you passed |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| that checkpoint. Keep doing this until get |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| moved back to spectator team! That is all! |', MESSAGE_COLOR_GAME);
        p.WriteConsole('| You  finished  a   whole  run!  Have  fun! |', MESSAGE_COLOR_GAME);
        p.WriteConsole('+--------------------------------------------+', MESSAGE_COLOR_GAME);
        p.WriteConsole('| Type !cmds for a list of commands          |', MESSAGE_COLOR_GOLD);
        p.WriteConsole('+--------------------------------------------+', MESSAGE_COLOR_GAME);
      end;
      '!bestrun': ShowBestRun(p);
      '!statistics': ShowStatistics(p);
      '!choosemap': StartChooseMap(Copy(Text, 12, Length(Text) - 11));
      '!votemap':   StartChooseMap(Copy(Text, 10, Length(Text) -  9));
      '!vote':      StartChooseMap(Copy(Text,  7, Length(Text) -  6));
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
        begin
          if IsQueueEmpty() then
            LoadReplay(p, Copy(Text, 9, Length(Text)))
          else
            if QueuePosition(p.ID) = 1 then
              LoadReplay(p, Copy(Text, 9, Length(Text)))
            else
              p.WriteConsole('[GAME] You cannot start a replay right now! Please wait for your turn to come!', MESSAGE_COLOR_RED);
        end else
          p.WriteConsole('[GAME] You cannot start a replay right now!', MESSAGE_COLOR_RED);
      end;
      '!search': PerformSearch(p, Text);
      else
        p.WriteConsole('[GAME] The command you have typed was invalid!', MESSAGE_COLOR_RED);
    end;
end;

function UpdateSpeedInfo(p: TRunnerProperties): TRunnerProperties;
begin
  if p.PPlayer <> NIL then
  begin
    if p.PositionArrayDistance = 60 then
      p.PositionArrayDistance := 1
    else
      p.PositionArrayDistance := p.PositionArrayDistance + 1;
    p.SumArrayDistance := p.SumArrayDistance - p.LastDistances[p.PositionArrayDistance];
    p.LastDistances[p.PositionArrayDistance] := Distance(p.LastPos.X, p.LastPos.Y, p.PPlayer.X, p.PPlayer.Y);
    p.SumArrayDistance := p.SumArrayDistance + p.LastDistances[p.PositionArrayDistance];
    p.LastPos.X := p.PPlayer.X;
    p.LastPos.Y := p.PPlayer.Y;
    Players.BigText(3, FormatFloat('0.000', (p.SumArrayDistance / 60) * 60 * 3600 / 10000) + ' km/h',
      MATH_SECOND_IN_TICKS * 2, MESSAGE_COLOR_GAME, 0.068, 5, 375);
    if RM.BestRunLoaded and not BotActive then
      if (Now - p.StartTime) > RM.BestRunLap[p.Laps].CheckPoint[p.CP] then
        Players.BigText(4, '+' + FormatDateTime('nn:ss.zzz', Now - p.StartTime - RM.BestRunLap[p.Laps].CheckPoint[p.CP]), MATH_SECOND_IN_TICKS * 2,
          MESSAGE_COLOR_RED,   0.068, 5, 390)
      else
        Players.BigText(4, '-' + FormatDateTime('nn:ss.zzz', RM.BestRunLap[p.Laps].CheckPoint[p.CP] - (Now - p.StartTime)), MATH_SECOND_IN_TICKS * 2,
          MESSAGE_COLOR_GREEN, 0.068, 5, 390);
  end;
  Result := p;
end;

procedure UniversalClockCalls(t: Integer);
begin
  if t mod (MATH_SECOND_IN_TICKS * 5) = 0 then
  begin
    if RM.Map.Loaded then
      DrawCheckPoints;
    if t mod (MATH_MINUTE_IN_TICKS * 15) = 0 then
      DB_Ping_Server;
    if ChooseMap.VoteInProgress then
    begin
      ChooseMap.Time_Remaining := ChooseMap.Time_Remaining - (MATH_SECOND * 5);
      if ChooseMap.Time_Remaining <= 0 then
      begin
        ChooseMap.VoteInProgress := False;
        WriteLnAndConsole(NIL, '[RM] No map has been voted.', MESSAGE_COLOR_GAME);
      end;
    end;
  end;
end;

procedure OnIdleTick(t: Integer);
begin
  UniversalClockCalls(t);
  if RM.Active then
  begin
    RM.Runner := UpdateSpeedInfo(RM.Runner);
    if BotActive then
    begin
      PassingCheckPoints();
      PlayBot();
    end else
    begin
      RecordKeys();
      PassingCheckPoints();
    end;
  end else
    if t mod 60 = 0 then
    begin
      if Game.NumPlayers - Game.NumBots > 0 then
      begin
        RM.TimeLeft := RM.TimeLeft - StrToDateTime(STR_TIME_SECOND);
        if RM.TimeLeft <= 0 then
        begin
          Map.SetMap(RM.Map.NextMap);
          SetWaitingTime(MATH_SECOND * 6);
        end else
          Players.BigText(5, 'Time Left: ' + FormatDateTime('nn:ss', RM.TimeLeft),
            MATH_SECOND_IN_TICKS * 3, MESSAGE_COLOR_GAME,
            0.068, 5, 405);
        DoQueueUpdateOnTick;
      end;
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
      Players.BigText(1, IntToStr(RM.Countdown div MATH_SECOND_IN_TICKS) + '...',
        MATH_SECOND_IN_TICKS * 2, Medal_Color_by_Rank(RM.Countdown div MATH_SECOND_IN_TICKS),
        0.068, 5, 360);
    RM.Countdown := RM.Countdown - 1;
  end else
  begin
    Game.OnClockTick := Pointers.Clock_Normal;
      Players.BigText(1, '0... Ready for next run!',
        MATH_SECOND_IN_TICKS * 2, MESSAGE_COLOR_GREEN,
        0.068, 5, 360);
    QueueDisplayTurn;
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
    if RM.Runner.PPlayer <> NIL then
      if Victim.ID = RM.Runner.PPlayer.ID then
        EndSingleGame(False);
end;

procedure EndGameAfterRespawn(p: TActivePlayer);
begin
  if RM.Active then
    if RM.Runner.PPlayer <> NIL then
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
      PlayerIP := DB_GetString(DB_ID, 2); // `lastip`
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
      if RM.Runner.PPlayer <> NIL then
        if p.ID = RM.Runner.PPlayer.ID then
          RM.Active := False;
  if p.Team <> TEAM_SPECTATOR then
    p.Team := TEAM_SPECTATOR;
  if p.Human then
  begin
    if RM.Map.Loaded then
      DrawCheckPoints;
    UpdatePlayerDatabase(p); // Adds the player if not in Database
    DB_PerformQuery(DB_ID, 'GameOnJoin', DB_Query_Replace_Val5(SQL_INSERT_ACTION,
      IntToStr(DB_PlayerGetIDbyHWID(p.HWID)), IntToStr(DB_SERVER_ID),
      IntToStr(ACTION_KIND_JOIN), '', ''));
    Achievement_Handle_Update(67, 1, p, False); // Addicted to you
    p.WriteConsole('[HELP] Welcome to !RunMode. Type !help if you are new.', MESSAGE_COLOR_SYSTEM);
  end;
end;

procedure GameOnLeave(p: TActivePlayer; Kicked: Boolean);
var
  I: Byte;
begin
  if RM.Active then
    if RM.Runner.PPlayer <> NIL then
      if p.ID = RM.Runner.PPlayer.ID then
        EndSingleGame(False);
  if p.IsAdmin then
    p.IsAdmin := False;
  RemoveFromQueue(p.ID);
  DB_PerformQuery(DB_ID, 'GameOnLeave', DB_Query_Replace_Val5(SQL_INSERT_ACTION,
    IntToStr(DB_PlayerGetIDbyHWID(p.HWID)), IntToStr(DB_SERVER_ID),
    IntToStr(ACTION_KIND_LEAVE), '', ''));
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
    if RM.Runner.PPlayer <> NIL then
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
    if RM.Runner.PPlayer <> NIL then
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
    RM.Runner.CP := 0;
    RM.Runner.SumArrayDistance := 0;
    RM.Runner.PositionArrayDistance := 1;
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
    if RM.Runner.PPlayer <> NIL then
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
    if RM.Runner.PPlayer <> NIL then
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
    if RM.Runner.PPlayer <> NIL then
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
  // TODO: LOG ERRORS IN SEPARATE FILES AND NOT CONSOLE
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
    '/timer':
    begin
      if Length(Command) = 15 then
      begin
        try
          RM.TimeLeft := StrToDateTime(Copy(Command, 8, Length(Command) - 7));
          WriteLnAndConsole(NIL, '[RM] Remaining time changed to ' + Copy(Command, 8, Length(Command) - 7) + '.', MESSAGE_COLOR_GAME);
        except
          DecideIfWriteLnOrConsole(p, '[RM] The input ''' + Copy(Command, 8, Length(Command) - 7) + ''' is not a valid time.', MESSAGE_COLOR_RED);
        end;
      end else
        DecideIfWriteLnOrConsole(p, '[RM] The input ''' + Copy(Command, 8, Length(Command) - 7) + ''' needs to have the format ''HH:MM:SS''!', MESSAGE_COLOR_RED);
    end;
    '/reload':
    begin
      if RM.Active then
        DecideIfWriteLnOrConsole(p, '[RM] You cannot reload the map while somebody is running.', MESSAGE_COLOR_GAME)
      else
        LoadMapSettings(Game.CurrentMap);
    end;
    '/nextmap':
    begin
      Result := True;
      if RM.Active then
        DecideIfWriteLnOrConsole(p, '[RM] You cannot change the map while somebody is running.', MESSAGE_COLOR_GAME)
      else
      begin
        Map.SetMap(RM.Map.NextMap);
        SetWaitingTime(MATH_SECOND * 6);
      end;
    end;
    '/restart',
    '/map':
    begin
      if RM.Active then
      begin
        DecideIfWriteLnOrConsole(p, '[RM] You cannot change the map while somebody is running.', MESSAGE_COLOR_GAME);
        Result := True;
      end else
        SetWaitingTime(MATH_SECOND * 6);
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
    Players[i].OnVoteMapStart        := @DenyVoteMap;
    Players[i].OnVoteKickStart       := @DenyVoteKick;
    Players[i].OnVoteMap             := @CountChooseMapVote;
    Players[i].OnVoteKick            := NIL;
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