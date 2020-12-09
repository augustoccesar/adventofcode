import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Day2 {
    public static int generateOutput(int noun, int verb) throws IOException {
        String input = readInput().trim();
        List<Integer> inputArr = Stream.of(input.split(",")).map(Integer::parseInt).collect(Collectors.toList());

        inputArr.set(1, noun);
        inputArr.set(2, verb);

        int opcode = -1;
        int i = 0;
        while (opcode != 99) {
            opcode = inputArr.get(i);

            int targetIdx = inputArr.get(i + 3);
            int firstValue = inputArr.get(inputArr.get(i + 1));
            int secondValue = inputArr.get(inputArr.get(i + 2));

            if (opcode == 1) {
                inputArr.set(targetIdx, firstValue + secondValue);
            }

            if (opcode == 2) {
                inputArr.set(targetIdx, firstValue * secondValue);
            }

            i += 4;
        }

        return inputArr.get(0);
    }

    public static void partOne() throws IOException {
        int output = generateOutput(12, 2);

        System.out.println("Part One: " + output);
    }

    public static void partTwo() throws IOException {
        boolean found = false;
        int expected = 19690720;
        int noun = -1;
        int verb = -1;

        for (int i = 99; i >= 0 && !found; i--) {
            for (int j = 99; j >= 0 && !found; j--) {
                int res = generateOutput(i, j);
                if (res == expected) {
                    noun = i;
                    verb = j;
                    found = true;
                }
            }
        }

        int result = 100 * noun + verb;
        System.out.println("Part Two: " + result);
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
        final InputStream input = Day2.class.getResourceAsStream("input.txt");
        final BufferedReader br = new BufferedReader(new InputStreamReader(input));

        while (br.ready()) {
            stringBuilder.append(br.readLine()).append("\n");
        }

        return stringBuilder.toString();
    }

    private static void streamInput(StreamCallback streamCallback) throws IOException {
        final InputStream input = Day2.class.getResourceAsStream("input.txt");
        final BufferedReader br = new BufferedReader(new InputStreamReader(input));

        while (br.ready()) {
            streamCallback.call(br.readLine());
        }
    }
}
