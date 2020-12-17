import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.concurrent.atomic.AtomicInteger;

public class Day1 {
    public static void partOne() throws IOException {
        final AtomicInteger sum = new AtomicInteger(0);
        streamInput((s) -> {
            int mass = Integer.parseInt(s);

            int value = sum.get() + fuelRequired(mass);
            sum.set(value);
        });

        System.out.println("Part One: " + sum.toString());
    }

    public static void partTwo() throws IOException {
        final AtomicInteger sum = new AtomicInteger(0);
        streamInput((s) -> {
            int mass = Integer.parseInt(s);
            int fuelRequired = fuelRequired(mass);
            int stageSum = fuelRequired;

            while (true) {
                fuelRequired = fuelRequired(fuelRequired);
                if (fuelRequired <= 0) {
                    break;
                }

                stageSum += fuelRequired;
            }

            sum.set(sum.get() + stageSum);
        });

        System.out.println("Part Two: " + sum.toString());
    }

    private static int fuelRequired(int mass) {
        return (mass / 3) - 2;
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
        final InputStream input = Day1.class.getResourceAsStream("input.txt");
        final BufferedReader br = new BufferedReader(new InputStreamReader(input));

        while (br.ready()) {
            stringBuilder.append(br.readLine()).append("\n");
        }

        return stringBuilder.toString();
    }

    private static void streamInput(StreamCallback streamCallback) throws IOException {
        final InputStream input = Day1.class.getResourceAsStream("input.txt");
        final BufferedReader br = new BufferedReader(new InputStreamReader(input));

        while (br.ready()) {
            streamCallback.call(br.readLine());
        }
    }
}