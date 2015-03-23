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

procedure Achievement_Handle_Update(AchievementOrChainID, Progress: Integer; p: TActivePlayer; IsChain: Boolean);

implementation

function Achievement_Has_ID_Finished(AchievementID, PlayerID: Integer): Boolean;
begin
  Result := True;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val2(SQL_ACH_FIN, IntToStr(AchievementID), IntToStr(PlayerID))) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
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
      Players.WriteConsole(p.Name + ' has earned the achievement ''' + AchievementName + '''!', MESSAGE_COLOR_GOLD);
      
    end;
    DB_FinishQuery(DB_ID);
  end else
    WriteLn('[RM] Error in Achievement_Has_ID_Finished: Database is not connected!');
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

end.