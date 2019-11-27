unit log;

interface

const
	android_log_LibName = 'liblog.so';

	ANDROID_LOG_UNKNOWN	=0;
	ANDROID_LOG_DEFAULT	=1;
	ANDROID_LOG_VERBOSE	=2;
	ANDROID_LOG_DEBUG	=3;
	ANDROID_LOG_INFO	=4;
	ANDROID_LOG_WARN	=5;
	ANDROID_LOG_ERROR	=6;
	ANDROID_LOG_FATAL	=7;
	ANDROID_LOG_SILENT	=8;

type android_LogPriority=integer;
type apchar=array of pchar;

function __android_log_write(prio:longint; tag, text:pchar):longint; cdecl; external android_log_LibName name '__android_log_write';
//function __android_log_print(prio:longint; tag:pchar; fmt: array of pchar ):longint; cdecl; external android_log_LibName name '__android_log_print';
function __android_log_print(prio:longint; tag:pchar; fmt1, fmt2: pchar ):longint; cdecl; varargs; external android_log_LibName name '__android_log_print';
function LOGI(prio:longint;tag,text:pchar):longint; cdecl; varargs; external android_log_LibName name '__android_log_print';
procedure LOGW(Text: pchar);

  
implementation

procedure LOGW(Text: pchar);
begin
   __android_log_write(ANDROID_LOG_FATAL,'crap',text);
end;


end.