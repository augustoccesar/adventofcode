# frozen_string_literal: true

require_relative '../day'

class Day06 < Day
  def part_one
    run(days: 80)
  end

  def part_two
    run(days: 256)
  end

  private

  def run(days:)
    timers = parse_input(read_input)
    timers_arr = Array.new(9, 0)

    timers.each { |timer| timers_arr[timer] += 1 }

    days.times do
      timers_arr = tick(timers_arr)
    end

    timers_arr.reduce(&:+)
  end

  def tick(timers_arr)
    ticked_timers = Array.new(9, 0)

    timers_arr.each_with_index do |timer, i|
      if i.zero?
        ticked_timers[8] += timer
        ticked_timers[6] += timer
        next
      end

      ticked_timers[i - 1] += timer
    end

    ticked_timers
  end

  def parse_input(input)
    input.split(',').map(&:to_i)
  end
end
