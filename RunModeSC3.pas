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

  TPlayerProperties = record
    Spec, Alive, InGame, OffMap: boolean;
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

var
  RM: TGameVariables;
  //ActivePlayer: Array[1..32] of TPlayerProperties;
  HighID: Byte;
  ReplayValues: Array of TReplay;
  ReplayBot: TActivePlayer;
  BotActive: Boolean;
  CurrentLoop: Integer;
  Pointers: TPointers;
  WHITESPACES: Array of string;

implementation

procedure WriteLnAndConsole(p: TActivePlayer; Text: string; Color: Cardinal);
begin
  WriteLn(Text);
  if p = NIL then
    Players.WriteConsole(Text, Color)
  else
    p.WriteConsole(Text, Color);
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
        WriteLn('[DB] Error in Explode_ReplayData: ' + DB_Error());
      end;
    end else
    begin
      SetArrayLength(Result, 0);
      WriteLn('[DB] Could not open replay Database!');
      WriteLn('[DB] Error in Explode_ReplayData: ' + DB_Error());
    end;
  except
    SetArrayLength(Result, 0);
    DB_FinishQuery(DB_ID_REPLAYS);
    DB_Close(DB_ID_REPLAYS);
    WriteLn('[DB] Something failed in Explode_ReplayData! Closed replay Database!');
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
      Exit;
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_PLR_MEDALS, IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      GoldMedals   := DB_GetLong(DB_ID, 0); // `gold`
      SilverMedals := DB_GetLong(DB_ID, 1); // `silver`
      BronzeMedals := DB_GetLong(DB_ID, 2); // `bronze`
      Result       := True;
    end;
  end else
    WriteLn('[DB] Error in ExchangeMedal: Database is not connected!');
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
            DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_GOLDS, IntToStr(MedalCountGoldWinner + 1), IntToStr(PlayerWhoGets)));
            if (PlayerWhoLoses > 0) and (MedalCountGoldLoser > 0) then
              DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_GOLDS, IntToStr(MedalCountGoldLoser - 1), IntToStr(PlayerWhoLoses)));
          end;
          MEDAL_SILVER:
          begin
            DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_SILVERS, IntToStr(MedalCountSilverWinner + 1), IntToStr(PlayerWhoGets)));
            if (PlayerWhoLoses > 0) and (MedalCountSilverLoser > 0) then
            DB_PerformQuery(DB_ID,'ExchangeMedal',  DB_Query_Replace_Val2(SQL_UPDATE_SILVERS, IntToStr(MedalCountSilverLoser - 1), IntToStr(PlayerWhoLoses)));
          end;
          MEDAL_BRONZE:
          begin
            DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_BRONZES, IntToStr(MedalCountBronzeWinner + 1), IntToStr(PlayerWhoGets)));
            if (PlayerWhoLoses > 0) and (MedalCountBronzeLoser > 0) then
              DB_PerformQuery(DB_ID, 'ExchangeMedal', DB_Query_Replace_Val2(SQL_UPDATE_BRONZES, IntToStr(MedalCountBronzeLoser - 1), IntToStr(PlayerWhoLoses)));
          end;
        end;
      end;
  end else
    WriteLn('[DB] Error in ExchangeMedal: Database is not connected!');
end;

procedure NewBronzeMedal(NewPlayer, OldBronze: Integer);
begin
  ExchangeMedal(MEDAL_BRONZE, NewPlayer, OldBronze);
end;

procedure NewSilverMedal(NewPlayer, OldSilver, OldBronze: Integer);
begin
  ExchangeMedal(MEDAL_SILVER, NewPlayer, OldSilver);
  NewBronzeMedal(OldSilver, OldBronze);
end;

procedure NewGoldMedal(NewPlayer, OldGold, OldSilver, OldBronze: Integer);
begin
  ExchangeMedal(MEDAL_GOLD, NewPlayer, OldGold);
  NewSilverMedal(OldGold, OldSilver, OldBronze);
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
      WriteLn('[DB] Error in GetPlayerRank: ' + DB_Error());
      DB_FinishQuery(DB_ID);
    end;
  end else
  begin
    Result := 0;
    WriteLn('[DB] Error in GetPlayerRank: Database is not connected!');
  end;
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
        WriteLn('[DB] The runner has a saved run here... Checking if new run is better...');
        ExistingRunID := DB_GetLong(DB_ID, 0);                  // `ID`
        DataBaseTime  := StrToDateTime(DB_GetString(DB_ID, 1)); // `runtime`
        DB_FinishQuery(DB_ID);
        if RunnerTime < DataBaseTime then
        begin
          WriteLn('[DB] The new run is better.. Updating his run...');
          DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val2(SQL_UPDATE_RUN,
            FormatDateTime('hh:nn:ss.zzz', RunnerTime), IntToStr(ExistingRunID)));
          Result := ExistingRunID;
        end else
          WriteLn('[DB] The new run is worse.. Doing nothing.');
      end else
      begin
        WriteLn('[DB] The runner did not have a time yet! Adding a new one...');
        DB_FinishQuery(DB_ID);
        DB_PerformQuery(DB_ID, 'Save_RunData', DB_Query_Replace_Val3(SQL_ADD_RUN,
          IntToStr(RM.Map.MapID), IntToStr(PlayerID),
          FormatDateTime('hh:nn:ss.zzz', RunnerTime)));
        if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_RUN, IntToStr(PlayerID),
            IntToStr(RM.Map.MapID))) <> 0) and
           (DB_NextRow(DB_ID) <> 0) then
        begin
          Result := DB_GetLong(DB_ID, 0); // `ID`
          DB_FinishQuery(DB_ID);
        end;
      end;

      // Now check for any difference in medals rankings
      if Result > 0 then // Result is over 0 if something changed
      begin
        PlayerNewRank := GetPlayerRank(PlayerID, RM.Map.MapID);
        case PlayerNewRank of
          1: NewGoldMedal(PlayerID, GoldPlayer, SilverPlayer, BronzePlayer);
          2: NewSilverMedal(PlayerID, SilverPlayer, BronzePlayer);
          3: NewBronzeMedal(PlayerID, BronzePlayer);
        end;
      end;
    end else
    begin
      Result := 0;
      WriteLn('[DB] Player with HWID ' + HWID + ' was not found in Database!');
      WriteLn('[DB] Error in Save_RunData: ' + DB_Error());
      DB_FinishQuery(DB_ID);
      Exit;
    end;
  end else
  begin
    Result := 0;
    WriteLn('[DB] Could not save RunData! Database is not connected!');
    Exit;
  end;
end;

// Rather complex here.. Too much input to do this via CONSTANTS and replaces.
function Save_ReplayData(runID: Integer; ReplayData: Array of TReplay): Boolean;
var
  I: Integer;
  runIDString: string;
  QueryString: string;
begin
  Result := True;

  if DB_Open(DB_ID_REPLAYS, DB_CON_STRING_REPLAY, '', '', DB_Plugin_ODBC) <> 0 then
  begin
    WriteLn('[DB] Deleting replay if exists...');
    runIDString := IntToStr(runID);
    DB_PerformQuery(DB_ID_REPLAYS, 'Save_ReplayData', DB_Query_Replace_Val2(SQL_DELETE_REPLAY, Game.CurrentMap, runIDString));
    WriteLn('[DB] Inserting new replay data...');
    QueryString := 'INSERT INTO `' + Game.CurrentMap +
    '` (`runID`, `KeyUp`, `KeyLeft`, `KeyRight`, `KeyJetpack`, `KeyGrenade`,' +
    ' `KeyChangeWeap`, `KeyThrow`, `KeyCrouch`, `KeyProne`, `AimX`, `AimY`, `PosX`, `PosY`) VALUES' + FILE_NEWLINE;
    QueryString := QueryString + FILE_NEWLINE + '(' + runIDString + ', ' +
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
      QueryString := QueryString + ', ' + FILE_NEWLINE + '(' + runIDString + ', ' +
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
    DB_Close(DB_ID_REPLAYS);
    WriteLn('[DB] Finished... Database closed!');
  end else
  begin
    WriteLn('[DB] Could not open replay database!');
    Result := False;
  end;
end;

procedure LoadMapSettings(MapToLoad: string);
var
  I: Byte;
begin
  WriteLn('[RM] Starting to load map ' + MapToLoad + '...');
  RM.Map.Loaded := True;
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

        while (DB_NextRow(DB_ID) <> 0) do
        begin
          try
            I := DB_GetLong(DB_ID, 0) - 1; // `checkpointID` - array is zero based

            RM.Map.CheckPoints[I].X        := DB_GetFloat(DB_ID, 1); // `posX`
            RM.Map.CheckPoints[I].Y        := DB_GetFloat(DB_ID, 2); // `posY`
            RM.Map.CheckPoints[I].Distance := DB_GetFloat(DB_ID, 3); // `distance`

            RM.Map.CheckPoints[I].Checked  := False;
          except
            WriteLn('[DB] Error: Database has more checkpoints saved');
            WriteLn('[DB] as in the map was defined! Please fix this.');
            WriteLn('[DB] CheckpointsNum: ' + IntToStr(RM.Map.AmountOfCheckpoints));
            WriteLn('[DB] Database returns checkpointID: ' + IntToStr(I + 1));
          end;
        end;

        DB_FinishQuery(DB_ID);
        WriteLn('[RM] The Map ' + MapToLoad + ' was loaded successfully!');

        WriteLn('[DB] Looking for a possible nextmap...');
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
        WriteLn('[DB] Error in LoadMapSettings: ' + DB_Error());
        WriteLn('[RM] Error: The map ' + MapToLoad + ' could not been loaded!');
        DB_FinishQuery(DB_ID);
        Exit;
      end;
    end else
    begin
      RM.Map.Loaded := False;
      WriteLn('[RM] The map ' + MapToLoad + ' was not found in the Database! Please set it up.');
      WriteLn('[DB] Error in LoadMapSettings: ' + DB_Error());
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
  RunTime := Now - RM.Runner.StartTime; // if this is done multiple times later on, the results will be different.
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
    WriteLnAndConsole(NIL, '[RM] ' + RM.Runner.PPlayer.Name + ' has finished a run in ' + FormatDateTime('hh:nn:ss.zzz', RunTime), MESSAGE_COLOR_GAME);
    WriteLnAndConsole(NIL, '[RM] Saving ' + RM.Runner.PPlayer.Name + '''s data.. This may take up to 3 seconds...', MESSAGE_COLOR_GAME);
    Game.OnClockTick := Pointers.Clock_Wait_Time;
    RM.Countdown := MATH_SECOND_IN_TICKS * 3;
    RM.Active := True;
    if ReplayBot <> NIL then
      if RM.Runner.PPlayer.ID <> ReplayBot.ID then
      begin
        Result_Run_ID := Save_RunData(RM.Runner.PPlayer.HWID, RunTime);
        if Result_Run_ID > 0 then
        begin
          if Save_ReplayData(Result_Run_ID, ReplayValues) then
            WriteLn('[DB] Saved the replay!')
          else
            WriteLn('[DB] Failed to save the replay!');
        end;
      end;
  end else
  begin
    WriteLn('[RM] ' + RM.Runner.PPlayer.Name + ' stopped his run.');
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
      Players.WorldText(0, FormatDateTime('nn:ss.zzz', RunTime), MATH_SECOND_IN_TICKS * 2,
        MESSAGE_COLOR_GREEN, 0.068, RM.Map.CheckPoints[i].X - 65, RM.Map.CheckPoints[i].Y + 50);
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
      if RM.Runner.Laps = RM.Map.AmountOfLaps then
        EndSingleGame(True)
      else
        Players.WorldText(0, FormatDateTime('nn:ss.zzz', RunTime), MATH_SECOND_IN_TICKS * 2,
          MESSAGE_COLOR_GREEN, 0.068, RM.Map.CheckPoints[i].X - 65, RM.Map.CheckPoints[i].Y + 50);
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
          Players.WriteConsole('| #' + IntToStr(RankID) + WHITESPACES[22 - Length(IntToStr(RankID))] + ' | ' + PlayerName + WHITESPACES[Length(PlayerName) - 1] + ' | ' +
            DB_GetString(DB_ID, 3) + 's   | ' + DB_GetString(DB_ID, 4) + ' | [' + DB_GetString(DB_ID, 0) + ']', Medal_Color_by_Rank(RankID));
          RankID := RankID + 1;
        end;
        Players.WriteConsole('+------+--------------------------+-----------------+---------------------+', MESSAGE_COLOR_GAME);

        DB_FinishQuery(DB_ID);

      end else
      begin
        WriteLn('[RM] The Map with the ID ' + IntToStr(SearchedMapID) + ' was not found in the Database!');
        WriteLn('[DB] Error in ShowTop: ' + DB_Error());
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

// TODO: VISUALS... Put the search in a fancy box
procedure PerformSearch(p: TActivePlayer; Text: string);
var
  Text_Piece: TStringList;
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
          if Length(Text_Piece.Strings[2]) > 17 then
          begin
            p.WriteConsole('[RM] The map name you was searching for is too long!', MESSAGE_COLOR_RED);
            Exit;
          end;

          p.WriteConsole('[RM] These are the results of your map search:', MESSAGE_COLOR_GAME);
          if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_MAP_BY_N, DB_Escape_String(Text_Piece.Strings[2]))) <> 0) AND
             (DB_NextRow(DB_ID) <> 0) then
          begin
            p.WriteConsole('[->] ' + DB_GetString(DB_ID, 0), MESSAGE_COLOR_GAME); // `mapname`
            while DB_NextRow(DB_ID) <> 0 do
              p.WriteConsole('[->] ' + DB_GetString(DB_ID, 0), MESSAGE_COLOR_GAME); // `mapname`
            DB_FinishQuery(DB_ID);
          end else
          begin
            p.WriteConsole('[->] No map was found.', MESSAGE_COLOR_GAME);
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

          p.WriteConsole('[RM] These are the results of your player search:', MESSAGE_COLOR_GAME);
          if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_SEARCH_PLR_BY_N, DB_Escape_String(Text_Piece.Strings[2]))) <> 0) AND
             (DB_NextRow(DB_ID) <> 0) then
          begin
            // `ID` = 0 `name` = 1 `gold` = 2 `silver` = 3 `bronze` = 4
            p.WriteConsole('[->] ' + DB_GetString(DB_ID, 0) + ' - ' + DB_GetString(DB_ID, 1) + ' | Golds: ' + DB_GetString(DB_ID, 2) +
              ' | Silvers: ' + DB_GetString(DB_ID, 3) + ' | Bronzes: ' + DB_GetString(DB_ID, 4), MESSAGE_COLOR_GAME);
            while DB_NextRow(DB_ID) <> 0 do
              p.WriteConsole('[->] ' + DB_GetString(DB_ID, 0) + ' - ' + DB_GetString(DB_ID, 1) + ' | Golds: ' + DB_GetString(DB_ID, 2) +
                ' | Silvers: ' + DB_GetString(DB_ID, 3) + ' | Bronzes: ' + DB_GetString(DB_ID, 4), MESSAGE_COLOR_GAME);
            DB_FinishQuery(DB_ID);
          end else
          begin
            p.WriteConsole('[->] No map was found.', MESSAGE_COLOR_GAME);
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

procedure OnSpeak(p: TActivePlayer; Text: string);
begin
  if Text[1] = '!' then
    case LowerCase(ReplaceRegExpr('\s\s*[^\s]*', Text, '', False)) of
      '!play':
      begin
        if not RM.Active then
          p.Team := TEAM_RUNNER;
      end;
      '!fail':
      begin
        if RM.Active then
          if p.ID = RM.Runner.PPlayer.ID then
            EndSingleGame(False);
      end;
      '!freerun': p.Team := TEAM_FREERUNNER;
      '!top': ShowTop(Text);
      '!top10': ShowTop(Text);
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
      '!replay': LoadReplay(p, Copy(Text, 9, Length(Text)));
      '!search': PerformSearch(p, Text);
      else
        p.WriteConsole('[GAME] The command you have typed was invalid!', MESSAGE_COLOR_RED);
    end;
end;

procedure UniversalClockCalls(t: Integer);
begin
  if t mod (MATH_SECOND_IN_TICKS * 5) = 0 then
  begin
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
    RM.Active := False;
  end;
end;

procedure AfterMapChange(NewMap: String);
begin
  LoadMapSettings(NewMap);
end;

function OnDamage(Shooter, Victim: TActivePlayer; Damage: Integer; BulletID: Byte): Integer;
begin
  Result := Damage;
end;

procedure OnKill(Killer, Victim: TActivePlayer; BulletID: Byte);
begin

end;

procedure UpdatePlayerDatabase(p: TActivePlayer);
var
  PlayerName: string;
begin
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_PLAYER_NAME, p.HWID)) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      PlayerName := DB_GetString(DB_ID, 0); // `name`
      DB_FinishQuery(DB_ID);

      if PlayerName <> p.Name then
      begin
        DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val2(SQL_UPDATE_PLR_NAME, DB_Escape_String(p.Name), p.HWID));
        DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val2(SQL_LOG_NAMECHANGE, p.HWID, DB_Escape_String(p.Name)));
      end;

      DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val1(SQL_UPDATE_PLR_SEEN, p.HWID));
    end else
    begin
      WriteLn('[DB] Player with HWID ' + p.HWID + ' was not found in Database...');
      DB_FinishQuery(DB_ID);

      WriteLn('[DB] Adding ' + p.Name + ' to the Database...');
      DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val2(SQL_ADD_PLAYER, p.HWID, DB_Escape_String(p.Name)));
      DB_PerformQuery(DB_ID, 'UpdatePlayerDatabase', DB_Query_Replace_Val2(SQL_LOG_NAMECHANGE, p.HWID, DB_Escape_String(p.Name)));
    end;
  end else
    WriteLn('[DB] Could not check for the player! Database is not connected!');
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
  if not RM.Active then
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
      EndSingleGame(False);
  p.WriteConsole('[GAME] You are now freerunning.', MESSAGE_COLOR_GAME);
end;

procedure OnJoinTeamSpectator(p: TActivePlayer; Team: TTeam);
begin
  if RM.Active then
    if p.ID = RM.Runner.PPlayer.ID then
      EndSingleGame(False);
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

procedure AddMapToDatabase(p: TActivePlayer; Command: string);
begin
  // TODO: IMPLEMENT
end;

procedure AddCPToDatabase(p: TActivePlayer; Command: string);
begin
  // TODO: IMPLEMENT
end;

procedure SetLapsToDatabase(p: TActivePlayer; Command: string);
begin
  // TODO: IMPLEMENT
end;

procedure SetCPNumToDatabase(p: TActivePlayer; Command: string);
begin
  // TODO: IMPLEMENT
end;

procedure RemoveCPFromDatabase(p: TActivePlayer; Command: string);
begin
  // TODO: IMPLEMENT
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

procedure DecideIfWriteLnOrConsole(p: TActivePlayer; Text: string; Color: Cardinal);
begin
  if p = NIL then
    WriteLn(Text)
  else
    p.WriteConsole(Text, Color);
end;

function OnSharedAdminCommand(p: TActivePlayer; Command: string): Boolean;
begin
  case LowerCase(ReplaceRegExpr('\s\s*[^\s]*', Command, '', False)) of
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
    else
      Result := False;
  end;
  // After setting Result := True it won't be shown anymore. This is a workaround
  if Result then
    if p <> NIL then
      WriteLn('BLOCKED COMMAND: ' + Command + '(' + p.IP + '[' + p.Name + '])');
end;

function IsInEditorMode(p: TActivePlayer): Boolean;
begin
  Result := p.Team = TEAM_EDITOR;
end;

function OnInGameAdminCommand(p: TActivePlayer; Command: string): Boolean;
begin
  Result := False;
  case LowerCase(ReplaceRegExpr('\s\s*[^\s]*', Command, '', False)) of
    '/ac':
    begin
      Result := True;
      AdminChat(p, Command);
    end;
    '/edit':     p.Team := TEAM_EDITOR;
    '/addmap':   AddMapToDatabase(p, Command);
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
  Script.OnException                 := @ErrorHandler;
  HighID := 1;
  for i := 1 to 32 do
  begin
    Players[i].OnDamage              := @OnDamage;
    Players[i].OnSpeak               := @OnSpeak;
    Players[i].OnKill                := @OnKill;
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