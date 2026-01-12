import java.awt.Canvas;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Color;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferStrategy;

public final class MinimalWindow {

    // ---- Global state ----
    private static Frame frame;
    private static Canvas canvas;
    private static BufferStrategy bufferStrategy;
    private static Graphics graphics;
    private static boolean shouldClose = false;

    // ---- API ----

    public static void initWindow() {
        frame = new Frame("Minimal Java Window");
        canvas = new Canvas();

        frame.setSize(640, 480);
        frame.add(canvas);
        frame.setResizable(false);
        frame.setVisible(true);

        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                shouldClose = true;
            }
        });

        canvas.createBufferStrategy(2);
        bufferStrategy = canvas.getBufferStrategy();
    }

    public static void closeWindow() {
        frame.dispose();
    }

    public static void beginDrawing() {
        graphics = bufferStrategy.getDrawGraphics();

        // Clear screen
        graphics.setColor(Color.BLACK);
        graphics.fillRect(0, 0, canvas.getWidth(), canvas.getHeight());
    }

    public static void endDrawing() {
        graphics.dispose();
        bufferStrategy.show();
    }

    public static boolean windowShouldClose() {
        return shouldClose;
    }

    public static void drawRectangle(int x, int y, int w, int h) {
        graphics.setColor(Color.RED);
        graphics.fillRect(x, y, w, h);
    }

    // ---- Example usage ----

    public static void main(String[] args) {
        initWindow();

        int x = 0;

        while (!windowShouldClose()) {
            beginDrawing();

            drawRectangle(x, 200, 100, 50);
            x = (x + 2) % 640;

            endDrawing();

            try {
                Thread.sleep(16);
            } catch (InterruptedException ignored) {}
        }

        closeWindow();
    }
}
