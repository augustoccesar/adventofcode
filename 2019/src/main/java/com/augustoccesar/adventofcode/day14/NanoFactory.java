package com.augustoccesar.adventofcode.day14;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;

public class NanoFactory {

  private int consumedOres = 0;
  private final HashMap<String, Reaction> reactions;
  private final HashMap<String, Integer> leftovers = new HashMap<>();
  private final Deque<Order> orders = new ArrayDeque<>();

  private NanoFactory(HashMap<String, Reaction> reactions) {
    this.reactions = reactions;
  }

  public static NanoFactory fromSpec(final String spec) {
    HashMap<String, Reaction> reactions = new HashMap<>();

    for (final Reaction reaction : reactionsFromSpec(spec)) {
      reactions.put(reaction.getOutputChemical(), reaction);
    }

    return new NanoFactory(reactions);
  }

  public int produce(final int amount, final String chemical) {
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

  private void order(final int amount, final String chemical) {
    this.orders.push(new Order(amount, chemical));
  }

  private void processOrder(final Order order) {
    if (order.chemical().equals("ORE")) {
      this.consumedOres += order.amount();
      return;
    }

    int actualAmountNecessary = order.amount();
    if (this.leftovers.get(order.chemical()) != null) {
      int leftoverAmount = this.leftovers.get(order.chemical());
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
    int reactionsNecessary = recipe.reactionsNecessary();
    int leftoverAmount = recipe.leftover();
    this.leftovers.put(
        order.chemical(),
        this.leftovers.getOrDefault(order.chemical(), 0) + leftoverAmount
    );

    for (int i = 0; i < reaction.getInputChemicals().size(); i++) {
      this.order(
          reactionsNecessary * reaction.getInputAmounts().get(i),
          reaction.getInputChemicals().get(i)
      );
    }
  }

  private Recipe recipeFor(final int amount, final String chemical) {
    Reaction reaction = findReactionFor(chemical);

    int reactionAmount = amount / reaction.getOutputAmount();
    int mod = amount % reaction.getOutputAmount();
    int leftover = 0;
    if (mod != 0) {
      leftover = reaction.getOutputAmount() - mod;
      reactionAmount++;
    }

    return new Recipe(reaction, reactionAmount, leftover);
  }

  private static List<Reaction> reactionsFromSpec(final String spec) {
    final List<Reaction> reactions = new ArrayList<>();

    for (final String line : spec.split("\n")) {
      reactions.add(Reaction.from(line));
    }

    return reactions;
  }

  private Reaction findReactionFor(final String chemical) {
    final Reaction reaction = reactions.get(chemical);
    if (reaction == null) {
      throw new IllegalArgumentException(
          String.format("Reaction for chemical %s do not exists.", chemical)
      );
    }

    return reaction;
  }
}