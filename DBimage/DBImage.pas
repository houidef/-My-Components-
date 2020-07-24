{*******************************************************************}
{        Version : 1.0 Alpha                                        }
{                                                                   }
{        TDBImage: TDBImage control descendant featured with        }
{         for Commercial use will be : 100 DA.                      }
{       Copyright © 2020-2021 By HOUIDEF AEK                        }
{       www.CfpaMesra.com                                           }
{       ALL RIGHTS RESERVED                                         }
{                                                                   }
{*******************************************************************}

unit DBImage;

interface

uses Windows, SysUtils, Classes, Controls, Messages, Graphics, Db, DBCtrls,
Forms, Dialogs, ComCtrls;

type

  TDBTransparentImage = class(TDBImage)
  private
    fBackgroundPic: TBitmap;
    fFocusRect: boolean;
    fFocusColor: TColor;
    fOnMouseExit: TNotifyEvent;
    fOnMouseEnter: TNotifyEvent;
    fTransparent: boolean;
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    procedure SetOnMouseExit(const Value: TNotifyEvent);
    procedure SetFocusColor(const Value: TColor);
    procedure SetFocusRect(const Value: boolean);
    procedure SetBackgroundPic(const Value: TBitmap);
    function GetCanvas: TCanvas;
   protected
    procedure CMMouseEnter (var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseExit (var Msg: TMessage); message CM_MOUSELEAVE;
    procedure DoMouseEnter; virtual;
    procedure DoMouseExit; virtual;
    procedure Paint; override;
    procedure SetTransparent(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SizePicture(const NewWidth, NewHeight: integer);
    procedure SizePictureToFit;                     // useful when you insert a record
    property PictureCanvas: TCanvas read GetCanvas; // just to make life a little easier
  published
    property Picture; 
    property BackgroundPic: TBitmap read FBackgroundPic write SetBackgroundPic;
    property FocusRect: boolean read FFocusRect write SetFocusRect default True;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clWindowFrame;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write SetOnMouseEnter;
    property OnMouseExit: TNotifyEvent read fOnMouseExit write SetOnMouseExit;
    property Transparent: boolean read fTransparent
                                  write SetTransparent default True;
  end;

  TDBDateTimePicker = class(TDateTimePicker)
  private
    { private declarations }

    // ...
    FDataLink: TFieldDataLink;

    procedure dataChange(Sender: TObject);
    procedure updateData(Sender: TObject);

    // datasource property
    procedure setDataSource(Value: TDataSource);
    function  getDataSource: TDataSource;

    // datafield property
    procedure setDataField(const value : string);
    function  getDataField : string;

    // field linked to the control
    function  getField : TField;

    // enter & exit
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;

  protected
    { protected declarations }

  public
    { public declarations }

    // constructor & distructor
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    // change
    procedure Change; override;

    // field linked to the control
    property Field      : TField      read getField;

  published
    { published declarations }

    // data field
    property DataField  : string      read getDataField  write setDataField;
    // data source
    property DataSource : TDataSource read getDataSource write setDataSource;
  end;
  
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MyComponents', [TDBTransparentImage,TDBDateTimePicker]);
end;

{ TDBTransparentImage }

procedure TDBTransparentImage.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  DoMouseEnter;
end;

procedure TDBTransparentImage.CMMouseExit(var Msg: TMessage);
begin
  inherited;
  DoMouseExit;
end;

constructor TDBTransparentImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBackgroundPic := TBitmap.Create;
  fFocusRect := True;
  fFocusColor := clWindowFrame;
  fTransparent := True;
  Color := clBtnFace;
end;

destructor TDBTransparentImage.Destroy;
begin
  FBackgroundPic.Free;
  inherited Destroy;
end;

procedure TDBTransparentImage.DoMouseEnter;
begin
  if Assigned(fOnMouseEnter) then
    fOnMouseEnter(Self);
end;

procedure TDBTransparentImage.DoMouseExit;
begin
  if Assigned(fOnMouseExit) then
    fOnMouseExit(Self);
end;

function TDBTransparentImage.GetCanvas: TCanvas;
begin
  Result := Picture.Bitmap.Canvas;
end;

procedure TDBTransparentImage.Paint;
var
  R: TRect;
  S: string;
  DrawPict: TPicture;
  TempBmp: TBitmap;
begin
  if ((Picture.Graphic <> nil) or (csPaintCopy in ControlState)) then begin
    DrawPict := TPicture.Create;
    try
      if (csPaintCopy in ControlState) and (Field <> nil)
        {$IFDEF VER120}
          and Field.IsBlob
        {$ENDIF} then
        DrawPict.Assign(Field)
      else DrawPict.Assign(Picture);

      TempBmp := TBitmap.Create;
      try
        TempBmp.Height := ClientHeight;
        TempBmp.Width := ClientWidth;
        with TempBmp.Canvas do begin
          if fBackgroundPic.Empty then
          begin
            Brush.Style := bsSolid;
            Brush.Color := Color;
            FillRect(ClientRect);
          end
          else StretchDraw(ClientRect, fBackgroundPic);

          if not ((DrawPict.Graphic = nil) or DrawPict.Graphic.Empty) then begin
            if Stretch then
              SetRect(R, 0, 0, ClientWidth, ClientHeight)
            else begin
              SetRect(R, 0, 0, DrawPict.Width, DrawPict.Height);
              if Center then OffsetRect(R, (ClientWidth - DrawPict.Width) div 2,
                (ClientHeight - DrawPict.Height) div 2);
            end;

            if fTransparent and (DrawPict.Graphic is TBitmap) then
            begin
              CopyMode := cmSrcAnd;
              StretchDraw(R, DrawPict.Bitmap);
              CopyMode := cmSrcCopy;
            end
            else StretchDraw(R, DrawPict.Graphic);
          end;
        end; //with
        Canvas.Draw(0, 0, TempBmp);
      finally
        TempBmp.Free;
      end;
    finally
      DrawPict.Free;
    end;
  end
  else begin
    Canvas.Font := Font;
    Canvas.Brush.Color := Color;
    if (Field <> nil) then
      S := Field.DisplayLabel
    else S := Name;
    S := '(' + S + ')';
    R := ClientRect;
    Canvas.TextRect(R, (R.Right - Canvas.TextWidth(S)) div 2, (R.Bottom - Canvas.TextHeight(S)) div 2, S);
  end;

  if (Focused and fFocusRect) then begin
    Canvas.Brush.Color := fFocusColor;
    Canvas.FrameRect(ClientRect);
  end;
end;

procedure TDBTransparentImage.SetBackgroundPic(const Value: TBitmap);
begin
  fBackgroundPic.Assign(Value);
  Invalidate;
end;

procedure TDBTransparentImage.SetFocusColor(const Value: TColor);
begin
  fFocusColor := Value;
  Invalidate;
end;

procedure TDBTransparentImage.SetFocusRect(const Value: boolean);
begin
  fFocusRect := Value;
  Invalidate;
end;

procedure TDBTransparentImage.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  fOnMouseEnter := Value;
end;

procedure TDBTransparentImage.SetOnMouseExit(const Value: TNotifyEvent);
begin
  fOnMouseExit := Value;
end;

procedure TDBTransparentImage.SetTransparent(const Value: boolean);
begin
  fTransparent := Value;
  Invalidate;
end;

procedure TDBTransparentImage.SizePicture(const NewWidth, NewHeight: integer);
var
  OldHeight, OldWidth: integer;
  OldColour: TColor;
  OldBrushStyle: TBRushStyle;
  NewRect: TRect;
begin
  OldHeight := Picture.Bitmap.Height;
  OldWidth := Picture.Bitmap.Width;
  Picture.Bitmap.Height := NewHeight;
  Picture.Bitmap.Width := NewWidth;
  if (OldHeight < NewHeight) or (OldWidth < Width) then
  begin
    OldColour := PictureCanvas.Brush.Color;
    OldBrushStyle := PictureCanvas.Brush.Style;
    PictureCanvas.Brush.Style := bsSolid;
    PictureCanvas.Brush.Color := PictureCanvas.Pixels[0,0];
    if OldHeight < NewHeight then
    begin
      NewRect := Rect(0, OldHeight, NewWidth, NewHeight);
      PictureCanvas.FillRect(NewRect);
    end;
    if OldWidth < NewWidth then
    begin
      NewRect := Rect(OldWidth, 0, NewWidth, NewHeight);
      PictureCanvas.FillRect(NewRect);
    end;
    PictureCanvas.Brush.Style := OldBrushStyle;
    PictureCanvas.Brush.Color := OldColour;
  end;
end;

procedure TDBTransparentImage.SizePictureToFit;
begin
  SizePicture(Width, Height);
end;

constructor TDBDateTimePicker.create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDataLink                 := TFieldDataLink.Create;
  FDataLink.Control         := Self;
  FDataLink.OnDataChange    := DataChange;
  FDataLink.OnUpdateData    := UpdateData;
end;

destructor TDBDateTimePicker.destroy;
begin
  FDataLink.free;
  FDataLink := nil;

  inherited destroy;
end;

procedure TDBDateTimePicker.dataChange(Sender: TObject);
begin
  // is it a valid field ?
  if fDataLink.field <> nil then begin
    // set database date time
    dateTime := fDataLink.field.asDateTime;
  end;
end;

procedure TDBDateTimePicker.UpdateData(Sender: TObject);
begin
  // is it a valid field ?
  if fDataLink.field <> nil then begin
    // set current date to the database field 
    fDataLink.field.asDateTime := dateTime;
  end;
end;

function TDBDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBDateTimePicker.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBDateTimePicker.GetDataField: string;
begin
  result := fDataLink.FieldName;
end;

procedure TDBDateTimePicker.SetDataField(const Value: string);
begin
  fDataLink.FieldName := Value;
end;

function TDBDateTimePicker.getField : TField;
begin
  result := FDataLink.field;
end;

// change
procedure TDBDateTimePicker.change;
begin
  // a change as been done to the field on database
  if fDataLink.edit then begin
    // call inherited change
    inherited Change;

    // set modifed status on data link
    fDataLink.modified;
  end;
end;

procedure TDBDateTimePicker.CMEnter(var Message: TCMEnter);
begin
  // ...
  inherited;

  // ...
  FDataLink.CanModify;
end;

procedure TDBDateTimePicker.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;

  inherited;
end;

end.
