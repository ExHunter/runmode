{*****************************************}
{                                         }
{         achievements unit  v1.0         }
{                                         }
{       Copyright (c) 2015 ExHunter       }
{                                         }
{*****************************************}

unit achievements;

interface

uses
  constants, libdb;

function Achievement_Has_ID_Finished(AchievementID, PlayerID: Integer): Boolean;
procedure Achievement_Handle_Update(AchievementOrChainID, Progress: Integer; p: TActivePlayer; IsChain: Boolean);
procedure Achievement_Handle_Map(Kind: Byte; MapID: Integer; p: TActivePlayer; Data1, Data2: Variant);
function Map_Has_Achievement_Kind(Kind: Byte; Mask: Word): Boolean;

implementation

function Achievement_Has_ID_Finished(AchievementID, PlayerID: Integer): Boolean;
begin
  Result := True;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_ACH_FIN, IntToStr(AchievementID), IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
      Result := True
    else
      Result := False;
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Has_ID_Finished: Database is not connected!');
end;

function Achievement_Has_Chain_Finished(ChainID, PlayerID: Integer): Boolean;
begin
  Result := True;
  // SQL_ACH_CHN_FIN_1 if results nothing, then result false, if not:
  // SQL_ACH_CHN_FIN_2 if results something, chain has been finished.
  // otherwise database might be compromised.
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_ACH_CHN_FIN_1, IntToStr(ChainID), IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
      Result := False;
    DB_FinishQuery(DB_ID);

    // no need to further check, if chain is in progress.
    if not Result then
      Exit;

    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_ACH_CHN_FIN_2, IntToStr(ChainID), IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
      // if has none achievements done in that chain yet.
      if DB_GetLong(DB_ID, 0) = 0 then // COUNT(*)
        Result := False;
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Has_Chain_Finished: Database is not connected!');
end;

function Achievement_Is_In_Progress(AchievementOrChainID, PlayerID: Integer): Integer;
begin
  Result := 0;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_ACHIEVE_IS_PROG, IntToStr(AchievementOrChainID), IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
      Result := DB_GetLong(DB_ID, 0); // `AchievementID`
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Is_In_Progress: Database is not connected!');
end;

function Achievement_Get_AchievementID(ChainID, ChainNumber: Integer): Integer;
begin
  Result := 0;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_ACHIEVE_ID, IntToStr(ChainID), IntToStr(ChainNumber))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
      Result := DB_GetLong(DB_ID, 0); // `ID`
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Get_AchievementID: Database is not connected!');
end;

function Achievement_Criteria_Met(AchievementID, PlayerID: Integer): Boolean;
begin
  Result := False;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_GET_ACHIEVE_PROG, IntToStr(AchievementID), IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      // `Progress` = 0, `Requirement` = 1
      if DB_GetLong(DB_ID, 0) >= DB_GetLong(DB_ID, 1) then
        Result := True;
    end else
      WriteLn('[RM] Error in Achievement_Criteria_Met: Achievement ' + IntToStr(AchievementID) +
        ' is not in progress for player ' + IntToStr(PlayerID) + '!');
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Criteria_Met: Database is not connected!');
end;

procedure Achievement_Add_Progress(AchievementID, Progress, PlayerID: Integer);
begin
  DB_PerformConnectedQuery('Achievement_Add_Progress', DB_Query_Replace_Val3(SQL_ACHIEVE_PROG_ADD,
    IntToStr(Progress), IntToStr(AchievementID), IntToStr(PlayerID)));
end;

procedure Achievement_End_Progress(AchievementID, PlayerID: Integer);
begin
  DB_PerformConnectedQuery('Achievement_End_Progress', DB_Query_Replace_Val2(SQL_ACHIEVE_PROG_DEL,
    IntToStr(PlayerID), IntToStr(AchievementID)));
end;

procedure Achievement_Reward(AchievementID, PlayerID: Integer; p: TActivePlayer);
begin
  DB_PerformConnectedQuery('Achievement_Reward', DB_Query_Replace_Val2(SQL_ACHIEVE_REWARD_1,
    IntToStr(PlayerID), IntToStr(AchievementID)));
  DB_PerformConnectedQuery('Achievement_Reward', DB_Query_Replace_Val2(SQL_ACHIEVE_REWARD_2,
    IntToStr(PlayerID), IntToStr(AchievementID)));
end;

procedure Achievement_Start_Progress(AchievementID, PlayerID: Integer);
var
  ChainID: Integer;
  Requirement: Integer;
begin
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_ACH_CHAINREQ, IntToStr(AchievementID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      ChainID     := DB_GetLong(DB_ID, 0); // `chainID`
      Requirement := DB_GetLong(DB_ID, 1); // `Requirement`
      DB_FinishQuery(DB_ID);
      DB_PerformQuery(DB_ID, 'Achievement_Start_Progress', DB_Query_Replace_Val4(SQL_BGN_ACHIEVE_PROG,
        IntToStr(AchievementID), IntToStr(PlayerID), IntToStr(ChainID), IntToStr(Requirement)));
    end;
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Get_Next_Chain_AchievementID: Database is not connected!');
end;

procedure Achievement_Add_Next_Chain_If_Exists(AchievementID, PlayerID: Integer);
var
  NewAchievementID: Integer;
begin
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, SQL_ACHIEVE_NE_CHN_1) <> 0) and
       (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_ACHIEVE_NE_CHN_2,
         IntToStr(AchievementID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      NewAchievementID := DB_GetLong(DB_ID, 0); // `ID`
      Achievement_Start_Progress(NewAchievementID, PlayerID);
      DB_FinishQuery(DB_ID);

      // Add old requirement as progress
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_ACH_CHAINREQ, IntToStr(AchievementID))) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
        Achievement_Add_Progress(NewAchievementID, DB_GetLong(DB_ID, 1), PlayerID); // `Requirement`
    end;
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Add_Next_Chain_If_Exists: Database is not connected!');
end;

procedure Achievement_Display_Claim(AchievementID, PlayerID: Integer; p: TActivePlayer);
var
  AchievementName: string;
  AchievementPoints: string;
  AchievementFirstClaimer: string;
begin
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_ACHIEVE_INFO_1, IntToStr(AchievementID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      AchievementName         := DB_GetString(DB_ID, 0); // `Name`
      AchievementPoints       := DB_GetString(DB_ID, 1); // `Points`
      AchievementFirstClaimer := DB_GetString(DB_ID, 2); // `FirstPlayerID`
      if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_ACHIEVE_INFO_2, AchievementFirstClaimer)) <> 0) and
         (DB_NextRow(DB_ID) <> 0) then
        AchievementFirstClaimer := DB_GetString(DB_ID, 0); // `name`
      p.BigText(200, 'ACHIEVEMENT EARNED:' + FILE_NEWLINE +
                     FILE_NEWLINE +
                     'POINTS EARNED:' + FILE_NEWLINE +
                     FILE_NEWLINE +
                     'FIRST PLAYER:' + FILE_NEWLINE,
        MATH_SECOND_IN_TICKS * 10, MESSAGE_COLOR_SILVER,
        0.068, 75, 240);
      p.BigText(201, FILE_NEWLINE +
                     AchievementName + ' -' + FILE_NEWLINE +
                     FILE_NEWLINE +
                     AchievementPoints + FILE_NEWLINE +
                     FILE_NEWLINE +
                     AchievementFirstClaimer + FILE_NEWLINE,
        MATH_SECOND_IN_TICKS * 10, MESSAGE_COLOR_GOLD,
        0.068, 150, 240);

      DB_PerformQuery(DB_ID, 'Achievement_Display_Claim', DB_Query_Replace_Val5(SQL_INSERT_ACTION, IntToStr(PlayerID),
        IntToStr(DB_SERVER_ID), IntToStr(ACTION_KIND_ACHIEVE), IntToStr(AchievementID), ''));
      Players.WriteConsole(p.Name + ' has earned the achievement ''' + AchievementName + '''!', MESSAGE_COLOR_GOLD);
      
    end;
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Display_Claim: Database is not connected!');
end;

procedure Achievement_Handle_Update(AchievementOrChainID, Progress: Integer; p: TActivePlayer; IsChain: Boolean);
var
  PlayerID: Integer;
  UpdatingAchievementID: Integer;
begin
  PlayerID := DB_PlayerGetIDbyHWID(p.HWID);

  if IsChain then
  begin
    if Achievement_Has_Chain_Finished(AchievementOrChainID, PlayerID) then
      Exit;
  end else
    if Achievement_Has_ID_Finished(AchievementOrChainID, PlayerID) then
      Exit;
      

  UpdatingAchievementID := Achievement_Is_In_Progress(AchievementOrChainID, PlayerID);
  if UpdatingAchievementID = 0 then
  begin
    if IsChain then
      UpdatingAchievementID := Achievement_Get_AchievementID(AchievementOrChainID, 1)
    else
      UpdatingAchievementID := AchievementOrChainID;
    Achievement_Start_Progress(UpdatingAchievementID, PlayerID);
  end;

  Achievement_Add_Progress(UpdatingAchievementID, Progress, PlayerID);
  if Achievement_Criteria_Met(UpdatingAchievementID, PlayerID) then
  begin
    Achievement_Reward(UpdatingAchievementID, PlayerID, p);
    Achievement_Display_Claim(UpdatingAchievementID, PlayerID, p);
    Achievement_End_Progress(UpdatingAchievementID, PlayerID);
    Achievement_Add_Next_Chain_If_Exists(UpdatingAchievementID, PlayerID);
  end;
end;

function Achievement_Lagrange_84_Criteria_Met(RunID: Integer): Boolean;
begin
  Result := False;
  if DB_Open(DB_ID_REPLAYS, DB_CON_STRING_REPLAY, '', '', DB_Plugin_ODBC) <> 0 then
  begin
    if DB_Query(DB_ID_REPLAYS, DB_Query_Replace_Val2(SQL_GET_REPLAY, Game.CurrentMap, IntToStr(runID))) <> 0 then
    begin
      Result := True;
      while DB_NextRow(DB_ID_REPLAYS) <> 0 do
      begin
        // `Keys` - KeyJetpack mask
        if DB_GetLong(DB_ID_REPLAYS, 0) and BINARY_4 = BINARY_4 then
        begin
          DB_FinishQuery(DB_ID_REPLAYS);
          DB_Close(DB_ID_REPLAYS);
          Result := False;
          Exit;
        end;
      end;
      DB_FinishQuery(DB_ID_REPLAYS);
      DB_Close(DB_ID_REPLAYS);
    end else
      WriteLn('[RM] Error in Achievement_Lagrange_84_Criteria_Met: ' + DB_Error());
  end else
    WriteLn('[RM] Error in Achievement_Lagrange_84_Criteria_Met: Could not open replay Database!');
end;

procedure Achievement_Handle_Map(Kind: Byte; MapID: Integer; p: TActivePlayer; Data1, Data2: Variant);
begin
  // TODO: LOAD MAP IDs FROM DB INSTEAD OF HARDCODE? Or keep it like this for performance?
  //       Could make an achievement conditions table, which is loaded and processed here.
  case Kind of
    ACH_MAP_KIND_CP:
    // Data1 - CheckpointID - Byte
    // Data2 - Time - TDateTime
    begin
      case MapID of
        6: // inf_Moonshine
        begin
          if Data1 = 2 then // 2nd checkpoint
            if Data2 - StrToDateTime(STR_TIME_10_SECONDS) < 0 then
              Achievement_Handle_Update(86, 1, p, False); // The shining moon
        end;
        else
          WriteLn('[DB] MapID ''' + IntToStr(MapID) + ''' has no checkpoint achievements!');
      end;
    end;
    ACH_MAP_KIND_LAP:
    // Data1 - LapNum - Byte
    // Data2 - Time - TDateTime
    begin
      case MapID of
        1: // ctf_Ash
        begin
          if Data1 = 1 then // 1st lap
            if Data2 - StrToDateTime(STR_TIME_10_SECONDS) < 0 then
              Achievement_Handle_Update(82, 1, p, False); // Ash to ash, dust to dust
        end;
        else
          WriteLn('[DB] MapID ''' + IntToStr(MapID) + ''' has no lap achievements!');
      end;
    end;
    ACH_MAP_KIND_SPEED:
    // Data1 - km/h - Float
    begin
      case MapID of
        2: // ctf_Cobra
        begin
          if Data1 >= 175.0 then // 175 km/h
            Achievement_Handle_Update(85, 1, p, False); // Booster!
        end;
        9: // ctf_Blade
        begin
          if Data1 >= 250.0 then // 250 km/h
            Achievement_Handle_Update(88, 1, p, False); // Holiday on ice
        end;
        13: // freetime12
        begin
          if Data1 >= 150.0 then // 150 km/h
            Achievement_Handle_Update(81, 1, p, False); // Speedtrap!
        end;
        else
          WriteLn('[DB] MapID ''' + IntToStr(MapID) + ''' has no speed achievements!');
      end;
    end;
    ACH_MAP_KIND_FINISH:
    // Data1 - Time - TDateTime
    // Data2 - Variant (can be different for each map)
    begin
      case MapID of
        8: // Lagrange
        // Data2 - RunID - Integer
        begin
          if Achievement_Lagrange_84_Criteria_Met(Data2) then
            Achievement_Handle_Update(84, 1, p, False); // Cannonball is for noobs!
        end;
        10: // ctf_Kampf
        // Data2 - BestRunTime - TDateTime
        begin
          if Data1 - Data2 >= StrToDateTime('00:00:19.900') then
            Achievement_Handle_Update(87, 1, p, False); // I also like to live dangerously
        end;
        else
          WriteLn('[DB] MapID ''' + IntToStr(MapID) + ''' has no run finish achievements!');
      end;
    end;
    ACH_MAP_KIND_HP:
    // Data1 - HP - Integer
    begin
      case MapID of
        17: // Bridge
        begin
          if Data1 < 4 then
            Achievement_Handle_Update(83, 1, p, False); // Medic!
        end;
        else
          WriteLn('[DB] MapID ''' + IntToStr(MapID) + ''' has no HP achievements!');
      end;
    end;
    else
      WriteLn('[DB] Error in Achievement_Handle_Map: Kind was not found!');
  end;
end;

function Map_Has_Achievement_Kind(Kind: Byte; Mask: Word): Boolean;
begin
  case Kind of
    ACH_MAP_KIND_CP:     Result := Mask and BINARY_1 = BINARY_1;
    ACH_MAP_KIND_LAP:    Result := Mask and BINARY_2 = BINARY_2;
    ACH_MAP_KIND_SPEED:  Result := Mask and BINARY_3 = BINARY_3;
    ACH_MAP_KIND_FINISH: Result := Mask and BINARY_4 = BINARY_4;
    ACH_MAP_KIND_HP:     Result := Mask and BINARY_5 = BINARY_5;
    else
      Result := False;
  end;
end;

end.