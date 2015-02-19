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
  FILE_EXTENSION_RPLY  = '.rply'; // replay data - will be removed soon

  // SQL STUFF - USING LATER ON STRREPLACE TO REPLACE VAL<num>
  SQL_PING_SERVER      = '/* ping */ SELECT 1;';
  SQL_GET_RND_MAP      = 'SELECT `mapname` FROM `rm_maps` ORDER BY RAND() LIMIT 0,1;';
  SQL_GET_MAP_ID_BY_N  = 'SELECT `ID`, `checknum`, `roundnum` FROM `rm_maps` WHERE `mapname` = ''VAL1'' LIMIT 1;';
  SQL_GET_MAP_CPS      = 'SELECT `checkpointID`, `posX`, `posY`, `distance` FROM `rm_checkpoints` WHERE `mapID` = VAL1 AND `courseID` = 1 ORDER BY `checkpointID` ASC;';
  SQL_ADD_MAP          = 'INSERT INTO `rm_maps` (`mapname`, `capnum`, `checknum`, `roundnum`, `coursesnum`, `datecreated`) ' +
                                        'VALUES (''VAL1'', VAL2, VAL3, VAL4, VAL5, ''VAL6'');';
  SQL_GET_RUN          = 'SELECT `ID`, `runtime` FROM rm_mapstats WHERE `playerID` = VAL1 AND `mapID` = VAL2 LIMIT 1;';
  SQL_ADD_RUN          = 'INSERT INTO `rm_mapstats` (`mapID`, `playerID`, `courseID`, `runtime`) ' + 
                                            'VALUES (VAL1, VAL2, 1, ''VAL3'');';
  SQL_UPDATE_RUN       = 'UPDATE `rm_mapstats` SET `runtime` = ''VAL1'', `rundate` = NOW() WHERE `ID` = VAL2;';
  SQL_GET_TOP_X        = 'SELECT `rm_mapstats`.`ID`, `rm_mapstats`.`playerID`, `playerstats`.`name`, `rm_mapstats`.`runtime`, `rm_mapstats`.`rundate` ' +
                         'FROM `rm_mapstats`, `playerstats` ' +
                         'WHERE `playerstats`.`ID` = `rm_mapstats`.`playerID` ' +
                         'AND `rm_mapstats`.`mapID` = VAL1 ' +
                         'ORDER BY `rm_mapstats`.`runtime` ASC LIMIT VAL2;';
  SQL_GET_PLAYER_ID    = 'SELECT `ID` FROM `playerstats` WHERE `HWID` = ''VAL1'' LIMIT 1;';

  // SQLL = SQL LITE
  SQLL_REPLAY_TABLE    = 'CREATE TABLE IF NOT EXISTS replay (' +
                            'ID INTEGER NOT NULL PRIMARY KEY, ' +
                            'KeyUp INTEGER NOT NULL DEFAULT ''0'', ' +
                            'KeyLeft INTEGER NOT NULL DEFAULT ''0'', ' +
                            'KeyRight INTEGER NOT NULL DEFAULT ''0'', ' +
                            'KeyJetpack INTEGER NOT NULL DEFAULT ''0'', ' +
                            'KeyGrenade INTEGER NOT NULL DEFAULT ''0'', ' +
                            'KeyChangeWeap INTEGER NOT NULL DEFAULT ''0'', ' +
                            'KeyThrow INTEGER NOT NULL DEFAULT ''0'', ' +
                            'KeyCrouch INTEGER NOT NULL DEFAULT ''0'', ' +
                            'KeyProne INTEGER NOT NULL DEFAULT ''0'', ' +
                            'AimX INTEGER NOT NULL DEFAULT ''0'', ' +
                            'AimY INTEGER NOT NULL DEFAULT ''0'', ' +
                            'PosX REAL NOT NULL DEFAULT ''0'', ' +
                            'PosY REAL NOT NULL DEFAULT ''0''' +
                         ')';
  SQLL_GET_REPLAY      = 'SELECT KeyUp, KeyLeft, KeyRight, KeyJetpack, KeyGrenade, KeyChangeWeap, KeyThrow, KeyCrouch, KeyProne, AimX, AimY, PosX, PosY FROM replay;';

  PATH_REPLAYS         = 'data\REPLAYS\';
  
  DB_CONNECTION_STRING = 'DB_HNS';  // insert ODBC connection string to databasse
  DB_USER = '';                     // keep blank
  DB_PASS = '';                     // keep blank
  DB_ID = 0;                        // keep it for all files 0
  DB_ID_SQLLITE = 1;                // used for SQLLite replays loading
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

  REPLAY_BOT_NAME      = 'ReplayBot v0.1';

  LAYER_OFFSET_CPS     = 1;
  LAYER_OFFSET_TRACKER = 10;

  MESSAGE_COLOR_GAME   = $55CCFF;
  MESSAGE_COLOR_ADMIN  = $66CCFF;
  MESSAGE_COLOR_SYSTEM = $F63817;
  MESSAGE_COLOR_RED    = $FF0000;
  MESSAGE_COLOR_GREEN  = $00FF00;
  MESSAGE_COLOR_GOLD   = $FFD700;
  MESSAGE_COLOR_SILVER = $ACACAC;
  MESSAGE_COLOR_BRONZE = $CD7F32;

implementation

end.

