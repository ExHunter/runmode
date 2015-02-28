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
  SQL_ADD_MAP          = 'INSERT INTO `rm_maps` (`mapname`, `capnum`, `checknum`, `roundnum`, `coursesnum`, `datecreated`) ' +
                                        'VALUES (''VAL1'', VAL2, VAL3, VAL4, VAL5, NOW());';
  SQL_ADD_REPLAY_TABLE = 'INSERT INTO `rm_maps` (`mapname`, `capnum`, `checknum`, `roundnum`, `coursesnum`, `datecreated`) ' +
                                        'VALUES (''VAL1'', VAL2, VAL3, VAL4, VAL5, NOW());';
  SQL_DEL_MAP          = 'DELETE FROM `rm_maps` WHERE `ID` = VAL1';
  SQL_ADD_CP           = 'INSERT INTO `rm_checkpoints` (`mapID`, `courseID`, `checkpointID`, `posX`, `posY`, `distance`) VALUES (VAL1, 1, VAL2, VAL3, VAL4, VAL5)';
  SQL_DEL_CP           = 'DELETE FROM `rm_checkpoints` WHERE `mapID` = VAL1 AND `checkpointID` = VAL2 AND `courseID` = 1';
  SQL_GET_RUN          = 'SELECT `ID`, `runtime` FROM rm_mapstats WHERE `playerID` = VAL1 AND `mapID` = VAL2 LIMIT 1;';
  SQL_ADD_RUN          = 'INSERT INTO `rm_mapstats` (`mapID`, `playerID`, `courseID`, `runtime`) ' + 
                                            'VALUES (VAL1, VAL2, 1, ''VAL3'');';
  SQL_UPDATE_RUN       = 'UPDATE `rm_mapstats` SET `runtime` = ''VAL1'', `rundate` = NOW() WHERE `ID` = VAL2;';
  SQL_GET_TOP_X        = 'SELECT `rm_mapstats`.`ID`, `rm_mapstats`.`playerID`, `playerstats`.`name`, `rm_mapstats`.`runtime`, `rm_mapstats`.`rundate`, `rm_maps`.`recordnum`, `rm_maps`.`runsnum`, `rm_maps`.`failsnum` ' +
                         'FROM `rm_mapstats`, `playerstats`, `rm_maps` ' +
                         'WHERE `playerstats`.`ID` = `rm_mapstats`.`playerID` ' +
                         'AND `rm_maps`.`ID` = VAL1 ' +
                         'AND `rm_mapstats`.`mapID` = VAL1 ' +
                         'ORDER BY `rm_mapstats`.`runtime` ASC LIMIT VAL2;';
  SQL_GET_RANK_1_OF_2  = 'SET @rownum := 0;';
  SQL_GET_RANK_2_OF_2  = 'SELECT `rank`, `playerID` ' +
                         'FROM (SELECT @rownum := @rownum + 1 AS rank, `runtime`, `playerID` ' +
                               'FROM `rm_mapstats` WHERE `mapID` = VAL1 ORDER BY `runtime` ASC' +
                               ') as result WHERE `playerID` = VAL2;';
  SQL_GET_PLAYER_ID    = 'SELECT `ID` FROM `playerstats` WHERE `HWID` = ''VAL1'' LIMIT 1;';
  SQL_GET_PLAYER_NAME  = 'SELECT `name` FROM `playerstats` WHERE `HWID` = ''VAL1'' LIMIT 1;';
  SQL_GET_PLR_MEDALS   = 'SELECT `gold`, `silver`, `bronze` FROM `playerstats` WHERE `ID` = VAL1 LIMIT 1;';
  SQL_ADD_PLAYER       = 'INSERT INTO `playerstats` (`HWID`, `name`, `firstjoin`, `lastseen`) VALUES (''VAL1'', ''VAL2'', NOW(), NOW());';
  SQL_UPDATE_PLR_NAME  = 'UPDATE `playerstats` SET `name` = ''VAL1'' WHERE `HWID` = ''VAL2'';';
  SQL_UPDATE_PLR_SEEN  = 'UPDATE `playerstats` SET `lastseen` = NOW() WHERE `HWID` = ''VAL1'';';
  SQL_UPDATE_GOLDS     = 'UPDATE `playerstats` SET `gold` = VAL1 WHERE `ID` = VAL2;';
  SQL_UPDATE_SILVERS   = 'UPDATE `playerstats` SET `silver` = VAL1 WHERE `ID` = VAL2;';
  SQL_UPDATE_BRONZES   = 'UPDATE `playerstats` SET `bronze` = VAL1 WHERE `ID` = VAL2;';
  SQL_SEARCH_MAP_BY_N  = 'SELECT `mapname` FROM `rm_maps` WHERE `mapname` LIKE ''%VAL1%'' LIMIT 15;';
  SQL_SEARCH_PLR_BY_N  = 'SELECT `ID`, `name`, `gold`, `silver`, `bronze` FROM `playerstats` WHERE `name` LIKE ''%VAL1%'' LIMIT 15;';
  // `kind` = 1 is log type name change
  SQL_SEARCH_ALT_NAME  = 'SELECT `info`, cnt FROM (SELECT `info`, SUM(CASE WHEN `HWID` = ''VAL1'' THEN 1 ELSE 0 END) as cnt, `kind` ' +
                         'FROM `privateactivity` GROUP BY `info` HAVING `kind` = 1) AS lookuptable ' +
                         'WHERE cnt > 0 ORDER BY cnt DESC LIMIT 15;';
  SQL_LOG_NAMECHANGE   = 'INSERT INTO `privateactivity` (`HWID`, `log_time`, `kind`, `info`) VALUES (''VAL1'', NOW(), 1, ''VAL2'');';

  // SQL queries for replays
 SQL_CREATE_REPLAY_TBL = 'CREATE TABLE IF NOT EXISTS `VAL1` ( ' +
                         '    `replayOrder` INT(11) NOT NULL AUTO_INCREMENT, ' +
                         '    `runID` INT(11) NOT NULL DEFAULT ''0'', ' +
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
                         '    PRIMARY KEY (`replayOrder`) ' +
                         ') ' +
                         'ENGINE=InnoDB;';
  SQL_DELETE_REPLAY    = 'DELETE FROM `VAL1` WHERE `runID` = VAL2;';
  SQL_GET_REPLAY       = 'SELECT `KeyUp`, `KeyLeft`, `KeyRight`, `KeyJetpack`, `KeyGrenade`, `KeyChangeWeap`, `KeyThrow`, `KeyCrouch`, `KeyProne`, `AimX`, `AimY`, `PosX`, `PosY` ' +
                         'FROM `VAL1` WHERE `runID` = VAL2 ORDER BY `replayOrder` ASC;';

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

  MEDAL_GOLD           = 1;
  MEDAL_SILVER         = 2;
  MEDAL_BRONZE         = 3;

  REPLAY_BOT_NAME      = 'ReplayBot v0.2';
  REPLAY_BOT_COL_PANTS = $000000;           // Black
  REPLAY_BOT_COL_SHIRT = $000000;           // Black
  REPLAY_BOT_COL_JETS  = $000000;           // Black
  REPLAY_BOT_COL_SKIN  = $000000;           // Black
  REPLAY_BOT_COL_HAIR  = $000000;           // Black
  REPLAY_BOT_STY_HAIR  = 1;                 // Dreadlocks
  REPLAY_BOT_STY_HEAD  = 0;                 // Nothing
  REPLAY_BOT_STY_CHAIN = 0;                 // Nothing

  LAYER_OFFSET_CPS     = 1;
  LAYER_OFFSET_TRACKER = 10;

  MESSAGE_COLOR_GAME   = $F5DEB3;
  MESSAGE_COLOR_ADMIN  = $66CCFF;
  MESSAGE_COLOR_SYSTEM = $F63817;
  MESSAGE_COLOR_RED    = $FF0000;
  MESSAGE_COLOR_GREEN  = $00FF00;
  MESSAGE_COLOR_GOLD   = $FFD700;
  MESSAGE_COLOR_SILVER = $ACACAC;
  MESSAGE_COLOR_BRONZE = $CD7F32;
  MESSAGE_COLOR_AC     = $FF8000;

  REGEXP_FIRST_WORD    = '\s\s*[^\s]*';

implementation

end.

