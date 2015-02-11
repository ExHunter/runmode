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
  FILE_EXTENSION_SQL   = '.sql'; // will be unused
  FILE_EXTENSION_INI   = '.ini'; // will be unused

  // SQL STUFF - USING LATER ON STRREPLACE TO REPLACE VAL<num>
  SQL_PING_SERVER      = '/* ping */ SELECT 1;';
  SQL_GET_RND_MAP      = 'SELECT `mapname` FROM `rm_maps` ORDER BY RAND() LIMIT 0,1;';
  SQL_GET_MAP_ID_BY_N  = 'SELECT `ID`, `checknum`, `roundnum` FROM `rm_maps` WHERE `mapname` = ''VAL1'' LIMIT 1;';
  SQL_GET_MAP_CPS      = 'SELECT `checkpointID`, `posX`, `posY`, `distance` FROM `rm_checkpoints` WHERE `mapID` = VAL1 AND `courseID` = 1 ORDER BY `checkpointID` ASC;';
  SQL_ADD_MAP          = 'INSERT INTO `rm_maps` (`mapname`, `capnum`, `checknum`, `roundnum`, `coursesnum`, `datecreated`)' +
                                        'VALUES (''VAL1'', VAL2, VAL3, VAL4, VAL5, ''VAL6'');';
  SQL_GET_RUN          = 'VAL1';
  SQL_ADD_RUN          = 'VAL1';
  SQL_GET_TOP          = 'VAL1';
  SQL_GET_PLAYER       = 'VAL1';

  PATH_CHECKPOINTS     = 'scripts/RunModeSC3/data/CHECKPOINTS/'; // will be unused
  PATH_SAVEDDATA       = '~\SQL\';                               // will be unused
  PATH_PATHTRACKER     = '~\PATHTRACKER\';                       // will be unused
  PATH_REPLAYS         = '~\REPLAYS\';                           // will be used
  PATH_LOGS            = '~\LOGS\';                              // will be unused
  
  DB_CONNECTION_STRING = 'DB_HNS';  // insert ODBC connection string to databasse
  DB_USER = '';                     // keep blank
  DB_PASS = '';                     // keep blank
  DB_ID = 0;                        // keep it for all files 0
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

implementation

end.

