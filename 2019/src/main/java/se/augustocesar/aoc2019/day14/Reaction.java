package se.augustocesar.aoc2019.day14;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
class Reaction {

  private static final Pattern REACTION_REGEX = Pattern.compile("(\\d+)\\s(\\w+)");

  @Getter
  private final List<String> inputChemicals;
  @Getter
  private final List<Long> inputAmounts;

  @Getter
  private final String outputChemical;
  @Getter
  private final long outputAmount;

  public static Reaction from(final String reactionInstruction) {
    final Matcher matcher = REACTION_REGEX.matcher(reactionInstruction);

    final List<String> chemicals = new ArrayList<>();
    final List<Long> amounts = new ArrayList<>();
    while (matcher.find()) {
      chemicals.add(matcher.group(2));
      amounts.add(Long.parseLong(matcher.group(1)));
    }

    final int lastIdx = chemicals.size() - 1;

    return new Reaction(
        chemicals.subList(0, lastIdx),
        amounts.subList(0, lastIdx),
        chemicals.get(lastIdx),
        amounts.get(lastIdx)
    );
  }
}