#define UNICODE
#define _UNICODE

#include <windows.h>
#include <windowsx.h>
#include <d2d1.h>
#include <stdlib.h>

#pragma comment(lib, "d2d1")

//#include "resource.h"
//<SnippetResource_H>
#define IDR_ACCEL1                      101
#define ID_TOGGLE_MODE                40002
#define ID_DRAW_MODE                  40003
#define ID_SELECT_MODE                40004
//</SnippetResource_H>

/* ============================================================
   Helpers
   ============================================================ */

#define SAFE_RELEASE(p) \
    if (p) { (p)->lpVtbl->Release(p); (p) = NULL; }

static float g_dpiScaleX = 1.0f;
static float g_dpiScaleY = 1.0f;

static void InitDPIScale(ID2D1Factory *factory)
{
    FLOAT dx, dy;
    factory->lpVtbl->GetDesktopDpi(factory, &dx, &dy);
    g_dpiScaleX = dx / 96.0f;
    g_dpiScaleY = dy / 96.0f;
}

static float PxToDipX(int x) { return x / g_dpiScaleX; }
static float PxToDipY(int y) { return y / g_dpiScaleY; }

/* ============================================================
   Ellipse
   ============================================================ */

typedef struct MyEllipse
{
    D2D1_ELLIPSE ellipse;
    D2D1_COLOR_F color;
    struct MyEllipse *next;
} MyEllipse;

static BOOL EllipseHitTest(MyEllipse *e, float x, float y)
{
    float a = e->ellipse.radiusX;
    float b = e->ellipse.radiusY;
    float dx = x - e->ellipse.point.x;
    float dy = y - e->ellipse.point.y;

    return ((dx * dx) / (a * a) + (dy * dy) / (b * b)) <= 1.0f;
}

/* ============================================================
   App State
   ============================================================ */

typedef enum
{
    MODE_DRAW,
    MODE_SELECT,
    MODE_DRAG
} Mode;

typedef struct
{
    HWND hwnd;
    HCURSOR cursor;

    ID2D1Factory          *factory;
    ID2D1HwndRenderTarget *rt;
    ID2D1SolidColorBrush  *brush;

    MyEllipse *ellipses;
    MyEllipse *selection;

    D2D1_POINT_2F mouse;
    Mode mode;
    size_t nextColor;
} AppState;

static D2D1_COLOR_F g_colors[] =
{
    { 1.0f, 1.0f, 0.0f, 1.0f },    /* Yellow */
    { 0.98f, 0.5f, 0.45f, 1.0f },  /* Salmon */
    { 0.2f, 0.8f, 0.2f, 1.0f }     /* Lime green */
};

/* ============================================================
   Graphics
   ============================================================ */

static HRESULT CreateGraphics(AppState *app)
{
    if (app->rt)
        return S_OK;

    RECT rc;
    GetClientRect(app->hwnd, &rc);

    D2D1_SIZE_U size = { rc.right, rc.bottom };

    D2D1_RENDER_TARGET_PROPERTIES rtp =
        D2D1_RENDER_TARGET_PROPERTIES_INIT(
            D2D1_RENDER_TARGET_TYPE_DEFAULT,
            D2D1_PIXEL_FORMAT_UNKNOWN,
            0.0f,
            0.0f,
            D2D1_RENDER_TARGET_USAGE_NONE,
            D2D1_FEATURE_LEVEL_DEFAULT
        );

    D2D1_HWND_RENDER_TARGET_PROPERTIES hwndProps =
        D2D1_HWND_RENDER_TARGET_PROPERTIES_INIT(app->hwnd, size);

    HRESULT hr = app->factory->lpVtbl->CreateHwndRenderTarget(
        app->factory,
        &rtp,
        &hwndProps,
        &app->rt
    );

    if (SUCCEEDED(hr))
    {
        D2D1_COLOR_F yellow = { 1, 1, 0, 1 };
        hr = app->rt->lpVtbl->CreateSolidColorBrush(
            app->rt,
            &yellow,
            NULL,
            &app->brush
        );
    }

    return hr;
}

static void DiscardGraphics(AppState *app)
{
    SAFE_RELEASE(app->rt);
    SAFE_RELEASE(app->brush);
}

static void Paint(AppState *app)
{
    if (FAILED(CreateGraphics(app)))
        return;

    PAINTSTRUCT ps;
    BeginPaint(app->hwnd, &ps);

    D2D1_COLOR_F clearColor = { 0.529f, 0.808f, 0.922f, 1.0f }; /* SkyBlue */

    app->rt->lpVtbl->BeginDraw(app->rt);
    app->rt->lpVtbl->Clear(app->rt, &clearColor);

    for (MyEllipse *e = app->ellipses; e; e = e->next)
    {
        app->brush->lpVtbl->SetColor(app->brush, &e->color);
        app->rt->lpVtbl->FillEllipse(app->rt, &e->ellipse, app->brush);

        D2D1_COLOR_F black = { 0,0,0,1 };
        app->brush->lpVtbl->SetColor(app->brush, &black);
        app->rt->lpVtbl->DrawEllipse(
            app->rt,
            &e->ellipse,
            app->brush,
            1.0f,
            NULL
        );
    }

    if (app->selection)
    {
        D2D1_COLOR_F red = { 1,0,0,1 };
        app->brush->lpVtbl->SetColor(app->brush, &red);
        app->rt->lpVtbl->DrawEllipse(
            app->rt,
            &app->selection->ellipse,
            app->brush,
            2.0f,
            NULL
        );
    }

    HRESULT hr = app->rt->lpVtbl->EndDraw(app->rt);
    if (FAILED(hr) || hr == D2DERR_RECREATE_TARGET)
        DiscardGraphics(app);

    EndPaint(app->hwnd, &ps);
}

/* ============================================================
   Input
   ============================================================ */

static void InsertEllipse(AppState *app, float x, float y)
{
    MyEllipse *e = calloc(1, sizeof(MyEllipse));
    if (!e) return;

    e->ellipse.point.x = x;
    e->ellipse.point.y = y;
    e->ellipse.radiusX = e->ellipse.radiusY = 2.0f;
    e->color = g_colors[app->nextColor++ % 3];

    e->next = app->ellipses;
    app->ellipses = e;
    app->selection = e;
}

static BOOL HitTest(AppState *app, float x, float y)
{
    for (MyEllipse *e = app->ellipses; e; e = e->next)
    {
        if (EllipseHitTest(e, x, y))
        {
            app->selection = e;
            return TRUE;
        }
    }
    return FALSE;
}

/* ============================================================
   Window Proc
   ============================================================ */

static LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM w, LPARAM l)
{
    AppState *app = (AppState *)GetWindowLongPtr(hwnd, GWLP_USERDATA);

    switch (msg)
    {
    case WM_CREATE:
    {
        app = calloc(1, sizeof(AppState));
        app->hwnd = hwnd;
        app->mode = MODE_DRAW;

        SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)app);

        D2D1CreateFactory(
            D2D1_FACTORY_TYPE_SINGLE_THREADED,
            &IID_ID2D1Factory,
            NULL,
            (void **)&app->factory
        );

        InitDPIScale(app->factory);
        return 0;
    }

    case WM_PAINT:
        Paint(app);
        return 0;

    case WM_SIZE:
        if (app->rt)
        {
            RECT rc;
            GetClientRect(hwnd, &rc);
            D2D1_SIZE_U size = { rc.right, rc.bottom };
            app->rt->lpVtbl->Resize(app->rt, &size);
        }
        return 0;

    case WM_DESTROY:
        DiscardGraphics(app);
        SAFE_RELEASE(app->factory);
        free(app);
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hwnd, msg, w, l);
}

/* ============================================================
   main()
   ============================================================ */

int main(void)
{
    HINSTANCE hInst = GetModuleHandleW(NULL);

    WNDCLASSW wc = {0};
    wc.lpfnWndProc = WndProc;
    wc.hInstance = hInst;
    wc.lpszClassName = L"CircleWindowClass";

    RegisterClassW(&wc);

    HWND hwnd = CreateWindowW(
        wc.lpszClassName,
        L"Draw Circles",
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, CW_USEDEFAULT,
        800, 600,
        NULL, NULL, hInst, NULL
    );

    ShowWindow(hwnd, SW_SHOWDEFAULT);

    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }

    return 0;
}
