# frozen_string_literal: true

require_relative "../task"

class Day06
  include Task

  def part_one
    timers = parse_input(read_input)
    timers_arr = Array.new(9, 0)

    timers.each { |timer| timers_arr[timer] += 1 }

    80.times do
      timers_arr = tick(timers_arr)
    end

    timers_arr.reduce(&:+)
  end

  def part_two
    "-"
  end

  private

  def tick(timers_arr)
    ticked_timers = Array.new(9, 0)

    timers_arr.each_with_index do |timer, i|
      if i.zero?
        ticked_timers[8] += timer
        ticked_timers[6] += timer
        next
      end

      ticked_timers[i-1] += timer
    end

    ticked_timers
  end

  def parse_input(input)
    input.split(",").map(&:to_i)
  end
end
