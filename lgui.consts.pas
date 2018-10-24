unit lgui.consts;

interface

uses
     System.UITypes, SynCommons2;

 const
LOG_COLORS: array[Boolean,TSynLogInfo] of TColor = (
    (TColors.White,$DCC0C0,$D0D0D0, $DCDCDC, $D0D0C0, $D0D0E0, TColors.Silver,$8080C0,$8080FF,$C0DCC0,$DCDCC0,
//  sllNone, sllInfo, sllEvent, sllDebug, sllIOWrite, sllOIRead, sllTrace, sllWarning, sllError, sllEnter, sllLeave,
     $C0C0F0, $C080FF, $C080F0, $C080C0, $C080C0,
//  sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
     $4040FF, $B08080, $B0B080, $8080DC, $80DC80, $DC8080, $DCFF00, $DCD000,
//  sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
     $DCDC80, $DC80DC, $DCDCDC,
//  sllServiceCall, sllServiceReturn, sllUserAuth,
      $D0D0DC),
//   sllCustom2
    (TColors.Black,TColors.Black,TColors.Black, TColors.Black, TColors.Black,TColors.Black,
    TColors.Black,TColors.Black,TColors.White,TColors.Black,TColors.Black,
     TColors.White,TColors.White,TColors.White,TColors.Black,TColors.Black,
     TColors.White,TColors.White,TColors.Black,TColors.White,
     TColors.Black,TColors.Black,TColors.Black,TColors.Black,
     TColors.Black,TColors.Black,TColors.Black,
     TColors.Black));
implementation

end.
