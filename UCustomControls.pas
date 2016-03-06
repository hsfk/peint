unit UCustomControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Graphics, Buttons, Controls;

procedure DefaultInit(AComponent, AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer);

type
  TACustomPanel = class(TPanel)
  public
    constructor Create(AParent: TComponent; ATop, ALeft, AWidth, AHeight: integer);
  end;

  TACustomLabel = class(TLabel)
  public
    constructor Create(AParent: TComponent; ATop, ALeft, AWidth, AHeight: integer;
      ACaption: string);
  end;

  TACustomButton = class(TButton)
  public
    constructor Create(AParent: TComponent; ATop, ALeft, AWidth, AHeight: integer;
      ACaption: string);
  end;

  TACustomSpeedButton = class(TSpeedButton)
  public
    constructor Create(AParent: TComponent; ATop, ALeft, AWidth, AHeight: integer;
      ACaption: string);
  end;

  TACustomShape = class(TShape)
  public
    constructor Create(AParent: TComponent; ATop, ALeft, AWidth, AHeight: integer;
      APenColor: TColor; APenStyle: TPenStyle; ABrushColor: TColor;
      ABrushStyle: TBrushStyle);
  end;

  TACustomCBox = class(TComboBox)
  public
    constructor Create(AParent: TComponent; ATop, ALeft, AWidth, AHeight: integer;
      AReadOnly: boolean);
  end;

  TACustomEdit = class(TEdit)
  public
    constructor Create(AParent: TComponent; ATop, ALeft, AWidth, AHeight: integer;
      ACaption: string; ANumbersOnly: boolean; AAlignment: TAlignment);
  end;

  TACustomSBar = class(TScrollBar)
  public
    constructor Create(AParent: TComponent;
      ATop, ALeft, AWidth, AHeight, AMin, AMax, APageSize: integer;
      AVisible: boolean);
  end;

implementation

procedure DefaultInit(AComponent, AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer);
begin
  TWinControl(AComponent).Parent := TWinControl(AParent);
  TWinControl(AComponent).Top := ATop;
  TWinControl(AComponent).Left := ALeft;
  TWinControl(AComponent).Width := AWidth;
  TWinControl(AComponent).Height := AHeight;
end;

constructor TACustomPanel.Create(AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer);
begin
  inherited Create(AParent);
  DefaultInit(Self, AParent, ATop, ALeft, AWidth, AHeight);
end;

constructor TACustomLabel.Create(AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer; ACaption: string);
begin
  inherited Create(AParent);
  DefaultInit(Self, AParent, ATop, ALeft, AWidth, AHeight);
  Self.Caption := ACaption;
end;

constructor TACustomButton.Create(AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer; ACaption: string);
begin
  inherited Create(AParent);
  DefaultInit(Self, AParent, ATop, ALeft, AWidth, AHeight);
  Self.Caption := ACaption;
end;

constructor TACustomSpeedButton.Create(AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer; ACaption: string);
begin
  inherited Create(AParent);
  DefaultInit(Self, AParent, ATop, ALeft, AWidth, AHeight);
  Self.Caption := ACaption;
end;

constructor TACustomShape.Create(AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer; APenColor: TColor;
  APenStyle: TPenStyle; ABrushColor: TColor; ABrushStyle: TBrushStyle);
begin
  inherited Create(AParent);
  DefaultInit(Self, AParent, ATop, ALeft, AWidth, AHeight);
  Self.Pen.Color := APenColor;
  Self.Pen.Style := APenStyle;
  Self.Brush.Color := ABrushColor;
  Self.Brush.Style := ABrushStyle;
end;

constructor TACustomCBox.Create(AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer; AReadOnly: boolean);
begin
  inherited Create(AParent);
  DefaultInit(Self, AParent, ATop, ALeft, AWidth, AHeight);
  Self.ReadOnly := AReadOnly;
end;

constructor TACustomEdit.Create(AParent: TComponent;
  ATop, ALeft, AWidth, AHeight: integer; ACaption: string; ANumbersOnly: boolean;
  AAlignment: TAlignment);
begin
  inherited Create(AParent);
  DefaultInit(Self, AParent, ATop, ALeft, AWidth, AHeight);
  Self.Caption := ACaption;
  Self.NumbersOnly := ANumbersOnly;
  Self.Alignment := AAlignment;
end;

constructor TACustomSBar.Create(AParent: TComponent;
  ATop, ALeft, AWidth, AHeight, AMin, AMax, APageSize: integer; AVisible: boolean);
begin
  inherited Create(AParent);
  DefaultInit(Self, AParent, ATop, ALeft, AWidth, AHeight);
  Self.Min := AMin;
  Self.Max := AMax;
  Self.PageSize := APageSize;
  Self.Visible := AVisible;
end;

end.
