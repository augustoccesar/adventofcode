package com.augustoccesar.adventofcode;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

public abstract class BaseDay {
    public abstract void partOne() throws IOException;

    public abstract void partTwo() throws IOException;

    public void run() {
        try {
            this.partOne();
            this.partTwo();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    protected interface StreamCallback {
        void call(String line);
    }

    protected String readInput() throws IOException {
        return readInput("input");
    }

    protected String readInput(final String name) throws IOException {
        final StringBuilder stringBuilder = new StringBuilder();
        final InputStream input = this.getClass().getResourceAsStream(resourceFolder() + "/" + name + ".txt");
        final BufferedReader br = new BufferedReader(new InputStreamReader(input));

        while (br.ready()) {
            stringBuilder.append(br.readLine()).append("\n");
        }

        return stringBuilder.toString();
    }

    protected void streamInput(StreamCallback streamCallback) throws IOException {
        final InputStream input = this.getClass().getResourceAsStream(resourceFolder() + "/input.txt");
        final BufferedReader br = new BufferedReader(new InputStreamReader(input));

        while (br.ready()) {
            streamCallback.call(br.readLine());
        }
    }

    private String resourceFolder() {
        return "/" + this.getClass().getSimpleName().toLowerCase();
    }
}
