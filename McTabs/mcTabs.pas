{*******************************************************************}
{        Version : 1.0 Alpha                                        }
{                                                                   }
{       TmcTabSet: TTabSet control descendant featured with         }
{         * Drag-n-drop tab reorder                                 }
{         * Inplace tab rename                                      }
{                                                                   }
{       Copyright © 2020-2021 By HOUIDEF AEK                        }
{       www.CfpaMesra.com                                           }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{*******************************************************************}

{$IFDEF VER150}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

unit mcTabs;

interface

uses
  Windows, Messages, SysUtils, Forms, Controls, Classes, StdCtrls, Graphics, Tabs, dialogs;

type
  TTabChangeNameEvent = procedure (Sender: TObject; TabIndex: Integer; var TabCaption: String; var Accept: Boolean)
    of object;


  TMCTabSet = class(TTabSet)
  private
    ixDragTab : Integer;
    bLMouseDn : Boolean;
    dtLMouseDn: DWORD;
    ptLMouseDn: TPoint; // in client's coords
    bDrawMarker: Boolean;

    FRenameTimeout: Cardinal;
    FOnChangeName : TTabChangeNameEvent;
    FOnChangeOrder: TTabChangeEvent;

    FPrevScrollClick: TNotifyEvent;
    procedure ScrollClick(Sender: TObject);

    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    function ItemFromMouse: Integer;
    procedure HideEditor;
  protected
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;

    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure Paint; override;
  public
    procedure DragDrop(Source: TObject; X, Y: Integer); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowEditor(Index: Integer);
  published
    property RenameTimeout: Cardinal read FRenameTimeout write FRenameTimeout
      default 700;
  end;
  TTabEdit = class(TCustomEdit)
  public
    Tabset:TMCTabSet;//f220
    index:Integer;//f224
    destructor Destroy; virtual;
    constructor Create(AOwner:TComponent); virtual;
    procedure DoExit; dynamic;
  end;
  function KeyboardHookProc(nCode: integer; wParam: WPARAM; lParam: LPARAM) : LRESULT; stdcall; 
  procedure Register;
var
  FTabEdit : TTabEdit;
  hk: HHook;
implementation

uses Types;

procedure Register;
begin
   RegisterComponents('MyComponents', [TMCTabSet]);
end;
{ TMCTabSet }

constructor TMCTabSet.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TMCTabSet.Destroy;
begin

  inherited;
end;

procedure TMCTabSet.DoStartDrag(var DragObject: TDragObject);
begin//0
  //00452F5C
  inherited;
  ixDragTab := ItemFromMouse;
  if (ixDragTab = -1) then
  begin//1
    //00452F90
    if (Tabs.Count > 0) then
    begin//2
      //00452FA2
      ixDragTab := Tabs.Count - 1;//EAX
      Exit;
    end;//2
  end;//1

  EndDrag(False);
end;//0


procedure TMCTabSet.DragDrop(Source: TObject; X, Y: Integer);
var
  I : integer;
begin//0
  //00452FD4
  inherited;
  I := ItemFromMouse;
 // ShowMessage(IntToStr(ixDragTab));

  if (I - 1 = ixDragTab) then
  begin//1
    //00453019
    if ( Tabs.Count - 1 = I) then  I:= -1   //00453036
    else   I := I + 1; //0045303F
  end;//1
  if (I = -1) then  I := Tabs.Count;  //00453050

  if ( ixDragTab = I) then Exit;
  if (ixDragTab < I) then  I := I - 1;   //0045307D

  Tabs.Move(ixDragTab, I);

  TabIndex := I;
end;//0


procedure TMCTabSet.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  i : integer;
begin
  inherited;
  I := ItemFromMouse;
  //ShowMessage(IntToStr(I));
 // Tabs.count;
 //ixDragTab := I + 1;
  {if (ixDragTab = I) then
  begin
     ShowMessage('ok');
  end; }
  Accept := true;
  bDrawMarker := True;
  Invalidate;
end;

procedure TMCTabSet.HideEditor;
begin//0
  //00453580
  if (FTabEdit <> Nil) then
      FreeAndNil(FTabEdit); //FTabEdit.Free;
end;//0


function TMCTabSet.ItemFromMouse: Integer;
var
  I,Sum: integer;
  Pos: TPoint;
  aRect : TRect;
begin//0
  //004535DC
  result := -1;
  Pos := ScreenToClient(Mouse.CursorPos);
  aRect := Bounds(0, 0, Width, Height);

  if (PtInRect(aRect, Pos)) then
  begin//1
    //0045363A
    Sum := StartMargin + 4;
      //00453677
      for I := FirstIndex to Tabs.Count - 1 do
      begin//3
        //0045367E
        if (Sum <= Width) then
        begin//4
          //00453686
          if (ItemWidth(I) + Sum + 9 >=  Pos.x) then
          begin//5
            //004536AA
            Result := I;
           Break;
          end;//5
        end;//4
        Sum := Sum + ItemWidth(I) + 9;
      end;//3
  end;//1
  //ShowMessage(IntToStr(Pos.x)+'/'+IntToStr(Pos.y)+'/'+IntToStr(Width)+'/'+IntToStr(Height)+':'+IntToStr(result));
end;//0


function KeyboardHookProc(nCode: integer; wParam: WPARAM;
  lParam: LPARAM): LRESULT;
var
  scan:integer;
  ks : TKeyboardState;
  w : Pansichar;
begin
   Result := CallNextHookEx(hk, nCode, wParam, lParam);
   if(ncode < 0) then exit;
  if(FTabEdit = nil) then Exit;

  if(ncode = HC_ACTION) and ((lParam and $40000000)=0) then
  case wParam of
  VK_RETURN :
    begin
       FTabEdit.Tabset.Tabs[FTabEdit.Tabset.TabIndex] := FTabEdit.Text;
       FreeAndNil(FTabEdit);
       //if()then FTabEdit.Free;
    end;
 // 14 :  FTabEdit.Free;
  19 :
    begin
      if(lParam shr 16 = $8000) then
          FTabEdit.Perform($101,wParam,lParam)
      else
       FTabEdit.Perform($100,wParam,lParam);
    end;
  end;

end;

procedure TMCTabSet.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin//0
  //00453168
 { if (FTabedit <> Nil) then
     FTabedit.Free; }


  inherited;
  if (Button =  mbLeft) and (not bLMouseDn ) then
  begin //004531B2
    dtLMouseDn := GetTickCount;
    //SetTimer(Handle, 1, FRenameTimeout, 0);
  end
  else
  begin
    bLMouseDn := true;
    ptLMouseDn := Point(X, Y);
  end;
end;//0


procedure TMCTabSet.MouseMove(Shift: TShiftState; X, Y: Integer);
begin//0
  //00453210
  inherited;
  //if (?f27C = 0) then Exit;
  if (ssLeft in Shift) then
  begin//1
    //00453243
    if (Abs(ptLMouseDn.X - X) <= 3) then
    begin//2
      //00453267
      if (Abs(ptLMouseDn.Y - Y) <= 3) then Exit;
    end;//2
    if (FTabEdit <> Nil) then
    begin//2
      //00453294
      if (FTabEdit = Nil) then Exit;
      if (FTabEdit.Tabset = Self) then Exit;
    end;//2
    BeginDrag(True, -1);
  end;//1
end;//0


procedure TMCTabSet.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin//0
  //004532C0
  inherited ;
  bLMouseDn := false;
  KillTimer(Handle, 1);
  if (Button = mbLeft) and ( ssCtrl in Shift) then//0045330B
    ShowEditor(TabIndex);
end;//0

procedure TMCTabSet.Paint;
var
  I: integer;
  ARect : TRect;
  P : array[1..3] of TPoint;
begin//0
  //00453388
  inherited;
  if (Dragging) then
  begin//1
    //004533A9
    if (Tabs.Count - 1 > 0) then
    begin//2
      //004533BE
      if (bDrawMarker = false) then Exit;
      I := itemfrommouse;
      if (I - 1 = ixDragTab) then
      begin//3
        //004533F1
        if (Tabs.Count - 1 = I) then
        begin//4
          //0045340E
          I := -1;
        end//4
        else
        begin//4
          //00453417
          I := I + 1;
        end;//4
      end;//3
      if (I = -1) then
      begin//3
        //00453428
        aRect := ItemRect(Tabs.Count - 1 - FirstIndex - FirstIndex) ;
        aRect.Left := aRect.Right + 5;
      end//3
      else
      begin//3
        //0045346F
        aRect := ItemRect(I - FirstIndex);
        aRect.left := aRect.left - 3;
      end;//3
      if (SoftTop) then
      begin//3
        //004534A4
        aRect.top := aRect.top - 1;
      end;//3
      Canvas.Pen.Color := $80;
      Canvas.Brush.Color := $FF;
      Canvas.Brush.Style := bsSolid;
      P[1] := Point(aRect.left - 4, aRect.top);
      P[2] := Point(aRect.left + 4, aRect.top);
      P[3] := Point(aRect.left, aRect.top + 7);
      Canvas.Polygon(P);
    end;//2
  end;//1
end;//0


procedure TMCTabSet.ScrollClick(Sender: TObject);
begin
  //===========
end;

procedure TMCTabSet.ShowEditor(Index: Integer);
begin//0
  //004535A0
  TabIndex := index;
  if (TabIndex <> -1) then
      FTabEdit := TTabEdit.Create(Self);
end;//0

procedure TMCTabSet.WMSize(var Message: TWMSize);
begin//0
  //00453364
  inherited;
  HideEditor;
end;//0


procedure TMCTabSet.WMTimer(var Message: TWMTimer);
begin//0
  //00453324
 // if (Dragging = False) then //0045333C
   //  ShowEditor(TabIndex);

  KillTimer(Handle, 1);
end;//0


{ TTabEdit }


constructor TTabEdit.Create(AOwner: TComponent);
var
  aRect : TRect;
begin
 inherited;
 Parent := TWinControl(AOwner);
 Tabset := TMCTabSet(Parent);
 index := Tabset.FirstIndex;
 aRect := Tabset.ItemRect(Tabset.TabIndex);
 SetBounds(aRect.Left,aRect.Top-2,aRect.Right-aRect.Left,aRect.Bottom - aRect.Top+1);
 Text := Tabset.Tabs[Tabset.TabIndex];
 SetFocus;


 hk := SetWindowsHookEx(WH_KEYBOARD,@KeyBoardHookProc,hInstance,GetCurrentThreadId);

end;

destructor TTabEdit.Destroy;
begin//0
  //00452DF0
  //gvar_00454D78 :=0;//EAX
  if (hk <> 0) then //00452E11
    UnhookWindowsHookEx(hk);
  hk := 0;//EAX
  inherited;
end;//0


procedure TTabEdit.DoExit;
begin
  inherited;
  Free;
end;

end.


