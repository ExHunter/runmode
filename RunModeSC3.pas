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

var
  RM: TGameVariables;
  //ActivePlayer: Array[1..32] of TPlayerProperties;
  HighID: Byte;
  ReplayValues: Array of TReplay;
  ReplayBot: TActivePlayer;
  BotActive: Boolean;
  CurrentLoop: Integer;

implementation

function Explode_RunData(Source: string): Array of TReplay;
var
  Position, DelLength_NEWLINE, DelLength_ROW, ResLength: integer;
begin
  DelLength_NEWLINE := Length(FILE_NEWLINE);
  DelLength_ROW     := Length(FILE_ROW);
  Source            := Source + FILE_NEWLINE;
  ResLength         := 0;
  repeat
    SetArrayLength(Result, ResLength + 1);
    // KeyUp
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyUp := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // KeyLeft
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyLeft := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // KeyRight
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyRight := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // KeyJetpack
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyJetpack := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // KeyGrenade
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyGrenade := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // KeyChangeWeap
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyChangeWeap := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // KeyThrow
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyThrow := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // KeyCrouch
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyCrouch := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // KeyProne
    Position := Pos(FILE_ROW, Source);
    Result[ResLength].KeyProne := iif(Copy(Source, 1, Position - 1) = '1', True, False);
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // AimX
    Position := Pos(FILE_ROW, Source);
    try
      Result[ResLength].AimX := StrToInt(Copy(Source, 1, Position - 1));
    except
      SetArrayLength(Result, 0);
      Exit;
    end;
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // AimY
    Position := Pos(FILE_ROW, Source);
    try
      Result[ResLength].AimY := StrToInt(Copy(Source, 1, Position - 1));
    except
      SetArrayLength(Result, 0);
      Exit;
    end;
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // PosX
    Position := Pos(FILE_ROW, Source);
    try
      Result[ResLength].PosX := StrToFloat(Copy(Source, 1, Position - 1));
    except
      SetArrayLength(Result, 0);
      Exit;
    end;
    Delete(Source, 1, Position + DelLength_ROW - 1);
    // PosY
    Position := Pos(FILE_ROW, Source);
    try
      Result[ResLength].PosY := StrToFloat(Copy(Source, 1, Position - 1));
    except
      SetArrayLength(Result, 0);
      Exit;
    end;
    Delete(Source, 1, Position + DelLength_ROW - 1);
    ResLength := ResLength + 1;
    Delete(Source, 1, Position + DelLength_NEWLINE - 1);
  until (Position = 0);
  SetArrayLength(Result, ResLength - 1);
end;

function Save_RunData(FileName: string; RunData: Array of TReplay): Boolean;
var
  I: Integer;
  RunData_File: TStringList;
begin
  Result := True;
  try
    RunData_File := File.CreateStringList();
    for I := 0 to GetArrayLength(RunData) - 1 do
      RunData_File.Append(iif(RunData[I].KeyUp, '1', '0') + FILE_ROW +
                          iif(RunData[I].KeyLeft, '1', '0') + FILE_ROW +
                          iif(RunData[I].KeyRight, '1', '0') + FILE_ROW +
                          iif(RunData[I].KeyJetpack, '1', '0') + FILE_ROW +
                          iif(RunData[I].KeyGrenade, '1', '0') + FILE_ROW +
                          iif(RunData[I].KeyChangeWeap, '1', '0') + FILE_ROW +
                          iif(RunData[I].KeyThrow, '1', '0') + FILE_ROW +
                          iif(RunData[I].KeyCrouch, '1', '0') + FILE_ROW +
                          IntToStr(RunData[I].AimX) + FILE_ROW +
                          IntToStr(RunData[I].AimY) + FILE_ROW +
                          IntToStr(RunData[I].PosX) + FILE_ROW +
                          IntToStr(RunData[I].PosY) + FILE_ROW);
    RunData_File.SaveToFile(PATH_REPLAYS + FileName + FILE_EXTENSION_RPLY);
  except
    Result := False;
  finally
    RunData_File.Free;
  end;
end;

procedure LoadMapSettings(MapToLoad: string);
var
  I: Byte;
  MapID: Integer;
begin
  WriteLn('[RM] Starting to load map ' + MapToLoad + '...');
  RM.Map.Loaded := True;
  if DB_CONNECTED then
  begin
    if (DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_ID_BY_N, MapToLoad)) <> 0) and
       (DB_NextRow(DB_ID) <> 0) then
    begin
      MapID                      := DB_GetLong(DB_ID, 0); // `ID`
      RM.Map.AmountOfCheckpoints := DB_GetLong(DB_ID, 1); // `checknum`
      RM.Map.AmountOfLaps        := DB_GetLong(DB_ID, 2); // `roundnum`
      SetArrayLength(RM.Map.CheckPoints, RM.Map.AmountOfCheckpoints);

      DB_FinishQuery(DB_ID);

      if MapID < 0 then
      begin
        RM.Map.Loaded := False;
        WriteLn('[RM] Could not find settings for the map ' + MapToLoad + '!');
        Exit;
      end;

      if DB_Query(DB_ID, DB_Query_Replace_Val1(SQL_GET_MAP_CPS, IntToStr(MapID))) <> 0 then
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
          I := DB_GetLong(DB_ID, 0) - 1; // `checkpointID` - array is zero based

          RM.Map.CheckPoints[I].X        := DB_GetFloat(DB_ID, 1); // `posX`
          RM.Map.CheckPoints[I].Y        := DB_GetFloat(DB_ID, 2); // `posY`
          RM.Map.CheckPoints[I].Distance := DB_GetFloat(DB_ID, 3); // `distance`

          RM.Map.CheckPoints[I].Checked  := False;
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

function AddReplayBot(): TActivePlayer;
var
  NewPlayer: TNewPlayer;
begin
  Result := NIL;
  NewPlayer := TNewPlayer.Create;
  try
    NewPlayer.Name := REPLAY_BOT_NAME;
    NewPlayer.Team := TEAM_SPECTATOR;
    NewPlayer.Dummy := True;
    NewPlayer.PantsColor := $FFFFFFFF;
    NewPlayer.SkinColor := $FFFFFFFF;
    Result := Players.Add(NewPlayer);
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
begin
  RunTime := Now - RM.Runner.StartTime; // if this is done multiple times later on, the results will be different.
  RM.Active := False;
  for j := 0 to RM.Map.AmountOfCheckpoints-1 do
    RM.Map.CheckPoints[j].Checked := False;
  RM.Runner.PPlayer.Team := TEAM_SPECTATOR;
  if Successfull then
  begin
    WriteLn('[RM] ' + RM.Runner.PPlayer.Name + ' has finished a run.');
    WriteLn('[RM] Time: ' + FormatDateTime('nn:ss.zzz', RunTime));
    Players.WriteConsole('[RM] ' + RM.Runner.PPlayer.Name + ' has finished a run in ' + FormatDateTime('nn:ss.zzz', RunTime), MESSAGE_COLOR_GAME);
    if DB_CONNECTED then
    begin
    
    end else
      WriteLn('[RM] Could not save ' + RM.Runner.PPlayer.Name + '''s run! Not connected to the Database!');
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
  ReplayBot.KeyUp := ReplayValues[CurrentLoop].KeyUp;
  ReplayBot.KeyLeft := ReplayValues[CurrentLoop].KeyLeft;
  ReplayBot.KeyRight := ReplayValues[CurrentLoop].KeyRight;
  ReplayBot.KeyJetpack := ReplayValues[CurrentLoop].KeyJetpack;
  ReplayBot.KeyGrenade := ReplayValues[CurrentLoop].KeyGrenade;
  ReplayBot.KeyChangeWeap := ReplayValues[CurrentLoop].KeyChangeWeap;
  ReplayBot.KeyThrow := ReplayValues[CurrentLoop].KeyThrow;
  ReplayBot.KeyCrouch := ReplayValues[CurrentLoop].KeyCrouch;
  ReplayBot.KeyProne := ReplayValues[CurrentLoop].KeyProne;
  ReplayBot.MouseAimX := ReplayValues[CurrentLoop].AimX;
  ReplayBot.MouseAimY := ReplayValues[CurrentLoop].AimY;
  ReplayBot.Move(ReplayValues[CurrentLoop].PosX, ReplayValues[CurrentLoop].PosY);
end;

procedure RecordKeys();
var
  Len: Integer;
begin
  Len := GetArrayLength(ReplayValues);
  SetArrayLength(ReplayValues, Len + 1);
  ReplayValues[Len].KeyUp := RM.Runner.PPlayer.KeyUp;
  ReplayValues[Len].KeyLeft := RM.Runner.PPlayer.KeyLeft;
  ReplayValues[Len].KeyRight := RM.Runner.PPlayer.KeyRight;
  ReplayValues[Len].KeyJetpack := RM.Runner.PPlayer.KeyJetpack;
  ReplayValues[Len].KeyGrenade := RM.Runner.PPlayer.KeyGrenade;
  ReplayValues[Len].KeyChangeWeap := RM.Runner.PPlayer.KeyChangeWeap;
  ReplayValues[Len].KeyThrow := RM.Runner.PPlayer.KeyThrow;
  ReplayValues[Len].KeyCrouch := RM.Runner.PPlayer.KeyCrouch;
  ReplayValues[Len].KeyProne := RM.Runner.PPlayer.KeyProne;
  ReplayValues[Len].PosX := RM.Runner.PPlayer.X;
  ReplayValues[Len].PosY := RM.Runner.PPlayer.Y;
  ReplayValues[Len].AimX := RM.Runner.PPlayer.MouseAimX;
  ReplayValues[Len].AimY := RM.Runner.PPlayer.MouseAimY;
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

procedure Thatbot();
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

procedure OnSpeak(p: TActivePlayer; Text: string);
begin
  if Text = '!play' then
    if not RM.Active then
      p.Team := TEAM_RUNNER;
  if Text = '!addbot' then
    Thatbot;
  if Text = '!replay' then
  begin
    BotActive := True;
    CurrentLoop := 0;
    ReplayBot.Team := TEAM_RUNNER;
  end;
end;

procedure OnIdleTick(t: integer);
begin
  if t mod 300 = 0 then
  begin
    DrawCheckPoints;
    if t mod 18000 = 0 then
      DB_Ping_Server;
  end;
  if RM.Active then
  begin
    if BotActive then
      PlayBot()
    else
      RecordKeys();
    PassingCheckPoints();
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

procedure GameOnJoin(p: TActivePlayer; Team: TTeam);
begin
  if p.ID > HighID then
    HighID := p.ID;
  if Team.ID = TEAM_RUNNER then
    if RM.Active then
      if p.ID = RM.Runner.PPlayer.ID then
        RM.Active := False;
  p.WriteConsole('[HELP] Welcome to !RunMode. Type !help if you are new.', MESSAGE_COLOR_SYSTEM);
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
      EndSingleGame(False);
  p.WriteConsole('[GAME] You cannot join this Team.', MESSAGE_COLOR_GAME);
  p.Team := TEAM_SPECTATOR;
end;

procedure OnJoinTeamRunner(p: TActivePlayer; Team: TTeam);
begin
  if RM.Active then
    if p.ID = RM.Runner.PPlayer.ID then
      EndSingleGame(False);
  if not RM.Active then
  begin
    if ReplayBot <> NIL then
      if p.ID <> ReplayBot.ID then
        SetArrayLength(ReplayValues, 0);
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
      EndSingleGame(False);
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
  p.WriteConsole('[GAME] You are now spectating. Type !play or !freerun to play.', MESSAGE_COLOR_GAME);
end;

procedure SetupRM();
var
  i: Byte;
begin
  WriteLn('[RM] Setting up RunMode3...');
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

  WriteLn('[RM] RunMode3 setup finished. Script runs now...');
  // on server startup this is empty, because map is not loaded yet.
  // used for recompile instead of restart map.
  if Game.CurrentMap <> '' then
    LoadMapSettings(Game.CurrentMap);
end;

initialization

  SetupRM;

end.