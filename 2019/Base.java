import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public class Base {
    public static void partOne() throws IOException {
    }

    public static void partTwo() throws IOException {
    }

    // -----------------------------------------------------------------------------------------------------------------

    public static void main(String[] args) throws IOException {
        partOne();
        partTwo();
    }

    private interface StreamCallback {
        void call(String line);
    }

    private static String readInput() throws IOException {
        final StringBuilder stringBuilder = new StringBuilder("");
        final InputStream input = Base.class.getResourceAsStream("input.txt");
        final BufferedReader br = new BufferedReader(new InputStreamReader(input));

        while(br.ready()) {
            stringBuilder.append(br.readLine()).append("\n");
        }

        return stringBuilder.toString();
    }

    private static void streamInput(StreamCallback streamCallback) throws IOException {
        final InputStream input = Base.class.getResourceAsStream("input.txt");
        final BufferedReader br = new BufferedReader(new InputStreamReader(input));

        while(br.ready()) {
            streamCallback.call(br.readLine());
        }
    }
}
