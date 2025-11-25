package com.augustoccesar.aocjava.y2019;

import com.augustoccesar.aocjava.Day;
import com.augustoccesar.aocjava.RunnableDay;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;

@RunnableDay(year = 2019, day = 14)
public class Day14 extends Day {
  @Override
  public String partOne() {
    NanoFactory nanoFactory = NanoFactory.fromSpec(this.readInputLines());
    long totalOfOres = nanoFactory.produce(1, "FUEL");

    return String.valueOf(totalOfOres);
  }

  @Override
  public String partTwo() {
    long target = 1_000_000_000_000L;
    NanoFactory nanoFactory = NanoFactory.fromSpec(this.readInputLines());

    int count = 1_000_000; // It can't be less than a million
    while (true) { // Do an outer loop jumping a lot
      long totalOfOres = nanoFactory.produce(count, "FUEL");
      if (totalOfOres > target) { // Once it passes the value, do a more granular inverse loop
        for (int i = count; ; i--) {
          totalOfOres = nanoFactory.produce(i, "FUEL");
          if (totalOfOres < target) {
            count = i;
            break;
          }
        }
        break;
      }

      count += 1_000;
    }

    return String.valueOf(count);
  }
}

record Order(long amount, String chemical) {}

record Recipe(Reaction reaction, long reactionsNecessary, long leftover) {}

@AllArgsConstructor(access = AccessLevel.PRIVATE)
class Reaction {

  private static final Pattern REACTION_REGEX = Pattern.compile("(\\d+)\\s(\\w+)");

  @Getter private final List<String> inputChemicals;
  @Getter private final List<Long> inputAmounts;

  @Getter private final String outputChemical;
  @Getter private final long outputAmount;

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
        amounts.get(lastIdx));
  }
}

class NanoFactory {

  private long consumedOres = 0;
  private final HashMap<String, Reaction> reactions;
  private final HashMap<String, Long> leftovers = new HashMap<>();
  private final Deque<Order> orders = new ArrayDeque<>();

  private NanoFactory(HashMap<String, Reaction> reactions) {
    this.reactions = reactions;
  }

  public static NanoFactory fromSpec(final List<String> spec) {
    HashMap<String, Reaction> reactions = new HashMap<>();

    for (final Reaction reaction : reactionsFromSpec(spec)) {
      reactions.put(reaction.getOutputChemical(), reaction);
    }

    return new NanoFactory(reactions);
  }

  public long produce(final long amount, final String chemical) {
    this.consumedOres = 0;
    this.leftovers.clear();
    this.orders.clear();

    this.order(amount, chemical);
    runUntilOutOfOrders();
    return this.consumedOres;
  }

  private void runUntilOutOfOrders() {
    while (!this.orders.isEmpty()) {
      Order order = this.orders.pop();
      this.processOrder(order);
    }
  }

  private void order(final long amount, final String chemical) {
    this.orders.push(new Order(amount, chemical));
  }

  private void processOrder(final Order order) {
    if (order.chemical().equals("ORE")) {
      this.consumedOres += order.amount();
      return;
    }

    long actualAmountNecessary = order.amount();
    if (this.leftovers.get(order.chemical()) != null) {
      long leftoverAmount = this.leftovers.get(order.chemical());
      if (leftoverAmount >= actualAmountNecessary) {
        // Leftovers can cover this order, so skip
        this.leftovers.put(order.chemical(), leftoverAmount - actualAmountNecessary);
        return;
      } else {
        // Will consume all leftover and still need more
        this.leftovers.remove(order.chemical());
        actualAmountNecessary -= leftoverAmount;
      }
    }

    Recipe recipe = recipeFor(actualAmountNecessary, order.chemical());

    Reaction reaction = recipe.reaction();
    long reactionsNecessary = recipe.reactionsNecessary();
    long leftoverAmount = recipe.leftover();
    this.leftovers.put(
        order.chemical(), this.leftovers.getOrDefault(order.chemical(), 0L) + leftoverAmount);

    for (int i = 0; i < reaction.getInputChemicals().size(); i++) {
      this.order(
          reactionsNecessary * reaction.getInputAmounts().get(i),
          reaction.getInputChemicals().get(i));
    }
  }

  private Recipe recipeFor(final long amount, final String chemical) {
    Reaction reaction = findReactionFor(chemical);

    long reactionAmount = amount / reaction.getOutputAmount();
    long mod = amount % reaction.getOutputAmount();
    long leftover = 0;
    if (mod != 0) {
      leftover = reaction.getOutputAmount() - mod;
      reactionAmount++;
    }

    return new Recipe(reaction, reactionAmount, leftover);
  }

  private static List<Reaction> reactionsFromSpec(final List<String> spec) {
    final List<Reaction> reactions = new ArrayList<>();

    for (final String line : spec) {
      reactions.add(Reaction.from(line));
    }

    return reactions;
  }

  private Reaction findReactionFor(final String chemical) {
    final Reaction reaction = reactions.get(chemical);
    if (reaction == null) {
      throw new IllegalArgumentException(
          String.format("Reaction for chemical %s do not exists.", chemical));
    }

    return reaction;
  }
}
