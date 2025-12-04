package se.augustocesar.aocjava.y2022;

import se.augustocesar.aocjava.Day;
import se.augustocesar.aocjava.RunnableDay;

@RunnableDay(year = 2022, day = 2)
public class Day02 extends Day {
  @Override
  public String partOne() {
    int totalScore =
        this.readInputLines().stream()
            .map(
                game -> {
                  var opponentHand = Hand.from(game.charAt(0));
                  var userHand = Hand.from(game.charAt(2));

                  Result result = userHand.playAgainst(opponentHand);

                  return userHand.points + result.points;
                })
            .mapToInt(Integer::intValue)
            .sum();

    return String.valueOf(totalScore);
  }

  @Override
  public String partTwo() {
    int totalScore =
        this.readInputLines().stream()
            .map(
                game -> {
                  var opponentHand = Hand.from(game.charAt(0));
                  var expectedResult = Result.from(game.charAt(2));

                  var userHand = Hand.from(expectedResult, opponentHand);

                  return userHand.points + expectedResult.points;
                })
            .mapToInt(Integer::intValue)
            .sum();

    return String.valueOf(totalScore);
  }
}

enum Result {
  WIN(6),
  DRAW(3),
  LOSS(0);

  public int points;

  private Result(int points) {
    this.points = points;
  }

  public static Result from(final char input) {
    return switch (input) {
      case 'X' -> Result.LOSS;
      case 'Y' -> Result.DRAW;
      case 'Z' -> Result.WIN;
      default -> throw new RuntimeException("Invalid character");
    };
  }
}

enum Hand {
  ROCK(1),
  PAPER(2),
  SCISSORS(3);

  public int points;

  private Hand(int points) {
    this.points = points;
  }

  public static Hand from(final char input) {
    return switch (input) {
      case 'A', 'X' -> Hand.ROCK;
      case 'B', 'Y' -> Hand.PAPER;
      case 'C', 'Z' -> Hand.SCISSORS;
      default -> throw new RuntimeException("Invalid character");
    };
  }

  /**
   * Returns the {@link Hand} necessary to have the expected {@link Result}.
   *
   * @param result expected {@link Result}
   * @param againstHand which {@link Hand} is the expected {@link Result} against
   * @return {@link Hand} to play to get the expected {@link Result}
   */
  public static Hand from(final Result result, final Hand againstHand) {
    return switch (result) {
      case Result.WIN -> againstHand.loseAgainst();
      case Result.DRAW -> againstHand;
      case Result.LOSS -> againstHand.winAgainst();
    };
  }

  public Hand winAgainst() {
    return switch (this) {
      case Hand.ROCK -> Hand.SCISSORS;
      case Hand.PAPER -> Hand.ROCK;
      case Hand.SCISSORS -> Hand.PAPER;
    };
  }

  public Hand loseAgainst() {
    return switch (this) {
      case Hand.ROCK -> Hand.PAPER;
      case Hand.PAPER -> Hand.SCISSORS;
      case Hand.SCISSORS -> Hand.ROCK;
    };
  }

  public Result playAgainst(final Hand opponent) {
    if (this == opponent) {
      return Result.DRAW;
    }

    if (this.winAgainst() == opponent) {
      return Result.WIN;
    } else {
      return Result.LOSS;
    }
  }
}
