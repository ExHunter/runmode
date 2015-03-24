{*****************************************}
{                                         }
{           constants unit v1.0           }
{                                         }
{       Copyright (c) 2015 ExHunter       }
{                                         }
{*****************************************}

unit constants;

interface

const

  MATH_TICKS           = 60;
  MATH_SECOND          = 1;
  MATH_MINUTE          = MATH_SECOND  * 60;
  MATH_HOUR            = MATH_MINUTE  * 60;
  MATH_DAY             = MATH_HOUR    * 24;
  MATH_WEEK            = MATH_DAY     * 7;
  MATH_30DAYS          = MATH_DAY     * 30;
  MATH_SECOND_IN_TICKS = MATH_SECOND  * MATH_TICKS;
  MATH_MINUTE_IN_TICKS = MATH_MINUTE  * MATH_TICKS;
  MATH_HOUR_IN_TICKS   = MATH_HOUR    * MATH_TICKS;
  MATH_DAY_IN_TICKS    = MATH_DAY     * MATH_TICKS;
  MATH_WEEK_IN_TICKS   = MATH_WEEK    * MATH_TICKS;
  MATH_30DAYS_IN_TICKS = MATH_30DAYS  * MATH_TICKS;
  MATH_TICK_IN_SECOND  = MATH_SECOND  / MATH_TICKS;

  FILE_ROW             = #9;
  FILE_NEWLINE         = #13#10;
  FILE_EXTENSION_SQL   = '.sql';
  FILE_EXTENSION_DB    = '.db';
  FILE_EXTENSION_INI   = '.ini';

  // SQL STUFF - USING LATER ON STRREPLACE TO REPLACE VAL<num>
  SQL_PING_SERVER      = '/* ping */ SELECT 1;';
  SQL_GET_RND_MAP      = 'SELECT `mapname` FROM `rm_maps` ORDER BY RAND() LIMIT 0,1;';
  SQL_GET_MAP_ID_BY_N  = 'SELECT `ID`, `checknum`, `roundnum` FROM `rm_maps` WHERE `mapname` = ''VAL1'' LIMIT 1;';
  SQL_GET_MAP_CPS      = 'SELECT `checkpointID`, `posX`, `posY`, `distance` FROM `rm_checkpoints` WHERE `mapID` = VAL1 AND `courseID` = 1 ORDER BY `checkpointID` ASC;';
  SQL_ADD_MAP          = 'INSERT INTO `rm_maps` (`mapname`, `datecreated`) ' +
                                        'VALUES (''VAL1'', NOW());';
  SQL_DEL_MAP          = 'DELETE FROM `rm_maps` WHERE `ID` = VAL1';
  SQL_SET_LAPS         = 'UPDATE `rm_maps` SET `roundnum` = VAL1 WHERE `ID` = VAL2;';
  SQL_SET_CPS          = 'UPDATE `rm_maps` SET `checknum` = VAL1 WHERE `ID` = VAL2;';
  SQL_ADD_CP           = 'INSERT INTO `rm_checkpoints` (`mapID`, `courseID`, `checkpointID`, `posX`, `posY`, `distance`) VALUES (VAL1, 1, VAL2, VAL3, VAL4, VAL5)';
  SQL_GET_CP_ID        = 'SELECT `checkpointID` FROM `rm_checkpoints` WHERE `mapID` = VAL1 AND `courseID` = 1 AND `checkpointID` = VAL2 LIMIT 1;';
  SQL_UPDATE_CP        = 'UPDATE `rm_checkpoints` SET `posX` = VAL1, `posY` = VAL2, `distance` = VAL3 WHERE `mapID` = VAL4 AND `checkpointID` = VAL5;';
  SQL_DEL_CP           = 'DELETE FROM `rm_checkpoints` WHERE `mapID` = VAL1 AND `checkpointID` = VAL2 AND `courseID` = 1';
  SQL_GET_RUN          = 'SELECT `ID`, `runtime` FROM rm_mapstats WHERE `playerID` = VAL1 AND `mapID` = VAL2 LIMIT 1;';
  SQL_ADD_RUN          = 'INSERT INTO `rm_mapstats` (`mapID`, `playerID`, `serverID`, `courseID`, `runtime`) ' + 
                                            'VALUES (VAL1, VAL2, VAL3, 1, ''VAL4'');';
  SQL_UPDATE_RUN       = 'UPDATE `rm_mapstats` SET `runtime` = ''VAL1'', `rundate` = NOW(), `serverID` = VAL2 WHERE `ID` = VAL3;';
  SQL_INC_RUNSNUM      = 'UPDATE `rm_maps` SET `runsnum` = -1 WHERE `ID` = VAL1;';  // works with SQL trigger in Database
  SQL_INC_FAILSNUM     = 'UPDATE `rm_maps` SET `failsnum` = -1 WHERE `ID` = VAL1;'; // works with SQL trigger in Database
  SQL_INC_RECORDNUM    = 'UPDATE `rm_maps` SET `recordnum` = -1 WHERE `ID` = VAL1;'; // works with SQL trigger in Database
  SQL_GET_TOP_X        = 'SELECT `rm_mapstats`.`ID`, `rm_mapstats`.`playerID`, `playerstats`.`name`, `rm_mapstats`.`runtime`, `rm_mapstats`.`rundate`, `rm_maps`.`recordnum`, `rm_maps`.`runsnum`, `rm_maps`.`failsnum` ' +
                         'FROM `rm_mapstats`, `playerstats`, `rm_maps` ' +
                         'WHERE `playerstats`.`ID` = `rm_mapstats`.`playerID` ' +
                         'AND `rm_maps`.`ID` = VAL1 ' +
                         'AND `rm_mapstats`.`mapID` = VAL1 ' +
                         'ORDER BY `rm_mapstats`.`runtime` ASC LIMIT VAL2;';
  SQL_GET_STATISTICS   = 'SELECT (SELECT COUNT(`ID`) FROM `rm_mapstats`) AS totalruns, ' +
                                '(SELECT COUNT(DISTINCT `playerID`) FROM `rm_mapstats`) AS differentplayers, ' +
                                '(SELECT SUM(`Points`) FROM `rm_achievements`) AS totalachievementpoints, ' +
                                '(SELECT COUNT(`ID`) FROM `rm_maps`) AS totalmaps, ' +
                                '(SELECT COUNT(`ID`) FROM `rm_achievements`) AS totaldifferentachievements, ' +
                                '(SELECT SUM(`failsnum`) FROM `rm_maps`) AS totalfails, ' +
                                '(SELECT SUM(`runsnum`) FROM `rm_maps`) AS totaltries ' +
                         'FROM `rm_maps`, `rm_mapstats`, `rm_achievements` ' +
                         'LIMIT 1;';
  SQL_GET_RANK_1_OF_2  = 'SET @rownum := 0;';
  SQL_GET_RANK_2_OF_2  = 'SELECT `rank`, `playerID` ' +
                         'FROM (SELECT @rownum := @rownum + 1 AS rank, `runtime`, `playerID` ' +
                               'FROM `rm_mapstats` WHERE `mapID` = VAL1 ORDER BY `runtime` ASC' +
                               ') as result WHERE `playerID` = VAL2;';
  SQL_GET_PLAYER_ID    = 'SELECT `ID` FROM `playerstats` WHERE `HWID` = ''VAL1'' LIMIT 1;';
  SQL_GET_ACTIVE_BANS  = 'SELECT `date`, `until`, `reason`, `admin` FROM `banlist` WHERE `until` > NOW() AND (`HWID` = ''VAL1'' OR `IP` = ''VAL2'') LIMIT 1;';
  SQL_GET_PLAYER_JOIN  = 'SELECT `name`, `adm`, `lastip` FROM `playerstats` WHERE `HWID` = ''VAL1'' LIMIT 1;';
  SQL_GET_PLR_MEDALS   = 'SELECT `gold`, `silver`, `bronze` FROM `playerstats` WHERE `ID` = VAL1 LIMIT 1;';
  SQL_GET_MAPSLIST     = 'SELECT `mapname` FROM `rm_maps` ORDER BY RAND() LIMIT 0,75;';
  SQL_ADD_PLAYER       = 'INSERT INTO `playerstats` (`HWID`, `name`, `firstjoin`, `lastseen`, `lastip`) VALUES (''VAL1'', ''VAL2'', NOW(), NOW(), ''VAL3'');';
  SQL_UPDATE_PLR_JOIN  = 'UPDATE `playerstats` SET `name` = ''VAL1'', `lastip` = ''VAL2'' WHERE `HWID` = ''VAL3'';';
  SQL_UPDATE_PLR_SEEN  = 'UPDATE `playerstats` SET `lastseen` = NOW() WHERE `HWID` = ''VAL1'';';
  SQL_UPDATE_GOLDS     = 'UPDATE `playerstats` SET `gold` = VAL1 WHERE `ID` = VAL2;';
  SQL_UPDATE_SILVERS   = 'UPDATE `playerstats` SET `silver` = VAL1 WHERE `ID` = VAL2;';
  SQL_UPDATE_BRONZES   = 'UPDATE `playerstats` SET `bronze` = VAL1 WHERE `ID` = VAL2;';
  SQL_SEARCH_MAP_BY_N  = 'SELECT `mapname` FROM `rm_maps` WHERE `mapname` LIKE ''%VAL1%'' LIMIT 15;';
  SQL_SEARCH_PLR_BY_N  = 'SELECT `ID`, `name`, `gold`, `silver`, `bronze` FROM `playerstats` WHERE `name` LIKE ''%VAL1%'' LIMIT 15;';
  SQL_SEARCH_PLR_BY_ID = 'SELECT `ID`, `name`, `gold`, `silver`, `bronze`, `firstjoin`, `lastseen` FROM `playerstats` WHERE `ID` = VAL1 LIMIT 1;';
  SQL_SEARCH_ALT_NAME  = 'SELECT `info`, cnt FROM (SELECT `info`, SUM(CASE WHEN `HWID` = ''VAL1'' THEN 1 ELSE 0 END) as cnt ' +
                         'FROM `namechanges` GROUP BY `info`) AS lookuptable ' +
                         'WHERE cnt > 0 ORDER BY cnt DESC LIMIT 15;';
  SQL_LOG_NAMECHANGE   = 'INSERT INTO `namechanges` (`HWID`, `IP`, `serverID`, `log_time`, `info`) VALUES (''VAL1'', ''VAL2'', VAL3, NOW(), ''VAL4'');';
  SQL_GET_ACHIEVE_ID   = 'SELECT `ID` FROM `rm_achievements` WHERE `chainNumber` = VAL1 AND `chainID` = VAL2 LIMIT 1;';
  SQL_ACHIEVE_IS_PROG  = 'SELECT `AchievementID` FROM `rm_achievements_progress` WHERE (`AchievementID` = VAL1 or `ChainID` = VAL1) AND `PlayerID` = VAL2 LIMIT 1;';
  SQL_GET_ACHIEVE_PROG = 'SELECT `Progress`, `Requirement` FROM `rm_achievements_progress` WHERE `AchievementID` = VAL1 AND `PlayerID` = VAL2 LIMIT 1;';
  SQL_ACHIEVE_PROG_ADD = 'UPDATE `rm_achievements_progress` SET `Progress` = VAL1 WHERE `AchievementID` = VAL2 AND `PlayerID` = VAL3 LIMIT 1;'; // works with SQL trigger in Database
  SQL_BGN_ACHIEVE_PROG = 'INSERT INTO `rm_achievements_progress` (`AchievementID`, `PlayerID`, `ChainID`, `Requirement`) VALUES (VAL1, VAL2, VAL3, VAL4);';
  SQL_GET_ACH_CHAINREQ = 'SELECT `chainID`, `Requirement` FROM `rm_achievements` WHERE `ID` = VAL1 LIMIT 1;';
  SQL_ACHIEVE_REWARD_1 = 'INSERT INTO `rm_achievements_claim` (`playerID`, `AchievementID`, `ClaimDate`) VALUES (VAL1, VAL2, NOW());';
  SQL_ACHIEVE_REWARD_2 = 'UPDATE `rm_achievements` SET `firstPlayerID` = VAL1 WHERE `ID` = VAL2 LIMIT 1;'; // works with SQL trigger in Database
  SQL_ACHIEVE_PROG_DEL = 'DELETE FROM `rm_achievements_progress` WHERE `PlayerID` = VAL1 AND `AchievementID` = VAL2 LIMIT 1;';
  SQL_ACHIEVE_NE_CHN_1 = 'SET @nextChain := 0;';
  SQL_ACHIEVE_NE_CHN_2 = 'SELECT `ID` ' +
                         'FROM (SELECT `chainID`, ' +
                         '             @nextChain := `chainNumber` + 1 as `nextChainNumber` ' +
                         '      FROM `rm_achievements` WHERE `ID` = VAL1 LIMIT 1) as `currAchievement`, ' +
                         '      `rm_achievements` ' +
                         'WHERE `currAchievement`.`chainID` = `rm_achievements`.`chainID` ' +
                         '      AND `currAchievement`.`nextChainNumber` = `rm_achievements`.`chainNumber` ' +
                         'LIMIT 1;';
  SQL_ACH_CHN_FIN_1    = 'SELECT `AchievementID` FROM `rm_achievements_progress` WHERE `ChainID` = VAL1 AND `PlayerID` = VAL2;';
  SQL_ACH_CHN_FIN_2    = 'SELECT COUNT(*) ' +
                         'FROM `rm_achievements` LEFT JOIN `rm_achievements_claim` ' +
                         'ON `rm_achievements`.`ID` = `rm_achievements_claim`.`AchievementID` ' +
                         'WHERE `rm_achievements_claim`.`playerID` = VAL2 AND `rm_achievements`.`chainID` = VAL1;';
  SQL_PLAYER_ACHIEVES  = 'SELECT SUM(`rm_achievements`.`Points`), COUNT(`rm_achievements`.`ID`) ' +
                         'FROM `rm_achievements` LEFT JOIN `rm_achievements_claim` ' +
                         'ON `rm_achievements`.`ID` = `rm_achievements_claim`.`AchievementID` ' +
                         'WHERE `rm_achievements_claim`.`playerID` = VAL1 ' +
                         'LIMIT 1;';
  SQL_ACHIEVE_RECENT   = 'SELECT `rm_achievements`.`Name`, `rm_achievements`.`Points`, `rm_achievements_claim`.`ClaimDate` ' +
                         'FROM `rm_achievements` LEFT JOIN `rm_achievements_claim` ' +
                         'ON `rm_achievements`.`ID` = `rm_achievements_claim`.`AchievementID` ' +
                         'WHERE `rm_achievements_claim`.`playerID` = VAL1 ' +
                         'ORDER BY `rm_achievements_claim`.`ClaimDate` DESC LIMIT 5;';
  SQL_ACH_FIN          = 'SELECT `AchievementID` FROM `rm_achievements_claim` WHERE `AchievementID` = VAL1 AND `playerID` = VAL2;';
  SQL_ACHIEVE_INFO_1   = 'SELECT `Name`, `Points`, `FirstPlayerID` FROM `rm_achievements` WHERE `ID` = VAL1 LIMIT 1;';
  SQL_ACHIEVE_INFO_2   = 'SELECT `name` FROM `playerstats` WHERE `ID` = VAL1 LIMIT 1;';
  SQL_RECENT_ACTIONS5  = 'SELECT `serverID`, `time`, `Kind`, `info` FROM `rm_activity` WHERE `PlayerID` = VAL1 ORDER BY `time` DESC LIMIT 5;';
  SQL_RECENT_ACTIONS15 = 'SELECT `serverID`, `time`, `Kind`, `info` FROM `rm_activity` WHERE `PlayerID` = VAL1 ORDER BY `time` DESC LIMIT 15;';
  SQL_INSERT_ACTION    = 'INSERT INTO `rm_activity` (`playerID`, `serverID`, `time`, `kind`, `info`) VALUES (VAL1, VAL2, NOW(), VAL3, ''VAL4'');';

  // SQL queries for replays
 SQL_CREATE_REPLAY_TBL = 'CREATE TABLE IF NOT EXISTS `VAL1` ( ' +
                         '    `replayOrder` SMALLINT(6) NOT NULL, ' +
                         '    `runID` INT(11) NOT NULL, ' +
                         '    `KeyUp` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `KeyLeft` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `KeyRight` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `KeyJetpack` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `KeyGrenade` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `KeyChangeWeap` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `KeyThrow` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `KeyCrouch` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `KeyProne` TINYINT(1) NOT NULL DEFAULT ''0'', ' +
                         '    `AimX` SMALLINT(6) NOT NULL DEFAULT ''0'', ' +
                         '    `AimY` SMALLINT(6) NOT NULL DEFAULT ''0'', ' +
                         '    `PosX` FLOAT NOT NULL DEFAULT ''0'', ' +
                         '    `PosY` FLOAT NOT NULL DEFAULT ''0'', ' +
                         '    UNIQUE INDEX `combination` (`replayOrder`, `runID`), ' +
                         '    INDEX `runID` (`runID`) ' +
                         ') ' +
                         'ENGINE=InnoDB;';
  SQL_CREATE_BESTRUN   = 'CREATE TABLE IF NOT EXISTS `VAL1_bestrun` ( ' +
                         '     `runID` INT(11) NOT NULL, ' +
                         '     `Lap` TINYINT(4) NOT NULL, ' +
                         '     `CheckPoint` TINYINT(4) NOT NULL, ' +
                         '     `Time` CHAR(12) NULL DEFAULT ''00:00:00.000'', ' +
                         '     UNIQUE INDEX `combination` (`runID`, `Lap`, `CheckPoint`), ' +
                         '     INDEX `ID` (`runID`) ' +
                         ') ' +
                         'COLLATE=''utf8_general_ci'' ' +
                         'ENGINE=InnoDB;';
  SQL_DELETE_REPLAY    = 'DELETE FROM `VAL1` WHERE `runID` = VAL2;';
  SQL_GET_REPLAY       = 'SELECT `KeyUp`, `KeyLeft`, `KeyRight`, `KeyJetpack`, `KeyGrenade`, `KeyChangeWeap`, `KeyThrow`, `KeyCrouch`, `KeyProne`, `AimX`, `AimY`, `PosX`, `PosY` ' +
                         'FROM `VAL1` WHERE `runID` = VAL2 ORDER BY `replayOrder` ASC;';
  SQL_DELETE_BESTRUN   = 'DELETE FROM `VAL1_bestrun` WHERE `runID` = VAL2;';
  SQL_GET_BESTRUN      = 'SELECT `runID`, `Lap`, `CheckPoint`, `Time` FROM `VAL1_bestrun` WHERE `runID` = VAL2';
  SQL_ADD_BESTRUN      = 'INSERT INTO `VAL1_bestrun` (`RunID`, `Lap`, `CheckPoint`, `Time`) VALUES (VAL2, VAL3, VAL4, ''VAL5'');';

  PATH_REPLAYS         = 'data\REPLAYS\';
  
  DB_CONNECTION_STRING = 'DB_HNS';  // insert ODBC connection string to database
  DB_USER = '';                     // keep blank
  DB_PASS = '';                     // keep blank
  DB_CON_STRING_REPLAY = 'DB_RM';   // insert ODBC connection string to replay database
  DB_ID = 0;                        // keep it for all files 0
  DB_ID_REPLAYS = 1;                // used for replays loading (different database)
  DB_SERVER_ID = 1;                 // used in database, hns eu has 2... rm na 3.. etc..

  TEAM_EDITOR          = 1;
  TEAM_VS              = 2;
  TEAM_RUNNER          = 3;
  TEAM_FREERUNNER      = 4;
  TEAM_SPECTATOR       = 5;

  GAME_MAXRUNNER       = 1;
  GAME_COUNTDOWN       = MATH_SECOND * 3;
  GAME_TIMELIMIT       = MATH_MINUTE * 3;

  QUEUE_TIMER          = MATH_SECOND * 10;
  QUEUE_COLOR_YELLOW   = MATH_SECOND * 5;
  QUEUE_COLOR_RED      = MATH_SECOND * 3;
  QUEUE_MAX_PLAYERS    = 9;

  MEDAL_GOLD           = 1;
  MEDAL_SILVER         = 2;
  MEDAL_BRONZE         = 3;

  ACTION_KIND_RUN      = 0;  // made a run          - info contains map and time
  ACTION_KIND_FAIL     = 1;  // failed a run        - info contains map
  ACTION_KIND_VS_WON   = 2;  // versus mode won     - info contains map and vs who
  ACTION_KIND_VS_LOST  = 3;  // versus mode lost    - info contains map and vs who
  ACTION_KIND_ACHIEVE  = 4;  // achievement claimed - info contains name and points
  ACTION_KIND_GOLD     = 5;  // got a gold medal    - info contains map
  ACTION_KIND_SILVER   = 6;  // got a silver medal  - info contains map
  ACTION_KIND_BRONZE   = 7;  // got a bronze medal  - info contains map
  ACTION_KIND_L_GOLD   = 8;  // lost a gold medal   - info contains map
  ACTION_KIND_L_SILVER = 9;  // lost a silver medal - info contains map
  ACTION_KIND_L_BRONZE = 10; // lost a bronze medal - info contains map

  REPLAY_BOT_NAME      = 'ReplayBot v0.2';
  REPLAY_BOT_COL_PANTS = $000000;           // Black
  REPLAY_BOT_COL_SHIRT = $000000;           // Black
  REPLAY_BOT_COL_JETS  = $000000;           // Black
  REPLAY_BOT_COL_SKIN  = $000000;           // Black
  REPLAY_BOT_COL_HAIR  = $000000;           // Black
  REPLAY_BOT_STY_HAIR  = 1;                 // Dreadlocks
  REPLAY_BOT_STY_HEAD  = 0;                 // Nothing
  REPLAY_BOT_STY_CHAIN = 0;                 // Nothing

  LAYER_OFFSET_CPS     = 5;

  MESSAGE_COLOR_GAME   = $F5DEB3;
  MESSAGE_COLOR_ADMIN  = $66CCFF;
  MESSAGE_COLOR_SYSTEM = $F63817;
  MESSAGE_COLOR_RED    = $FF0000;
  MESSAGE_COLOR_GREEN  = $00FF00;
  MESSAGE_COLOR_GOLD   = $FFD700;
  MESSAGE_COLOR_SILVER = $ACACAC;
  MESSAGE_COLOR_BRONZE = $CD7F32;
  MESSAGE_COLOR_AC     = $FF8000;

  REQUEST_STATE_OK     = 1;
  REQUEST_STATE_VER    = 2;
  REQUEST_STATE_PASS   = 3;
  REQUEST_STATE_BANNED = 4;
  REQUEST_STATE_FULL   = 5;
  REQUEST_STATE_DUP_IP = 6;

  VOTE_TIME            = MATH_SECOND * 20;
  VOTE_PERCENT         = 70.00;

  REGEXP_FIRST_WORD    = '\s\s*[^\s]*';

  STR_TIME_SECOND      = '00:00:01';
  STR_TIME_LIMIT       = '00:05:00';

implementation

end.

