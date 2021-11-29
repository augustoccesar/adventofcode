# frozen_string_literal: true

require 'benchmark'

day_str = ARGV[0]
day_str = "0#{day_str}" if day_str.to_i < 10

begin
  require "./day#{day_str}/day#{day_str}"
  day = Object.const_get("Day#{day_str}").new

  [[:part_one, "one"], [:part_two, "two"]].each do |items|
    method = items[0]
    name = items[1]

    result = nil
    elapsed_part_one = Benchmark.measure { result = day.send(method) }
    pretty_time = Time.at(elapsed_part_one.real).utc.strftime("%M:%S.%N")

    puts("Part #{name}: #{result}\n(took #{pretty_time})\n\n")
  end
rescue LoadError
  puts "Implementation for day #{day_str} not found."
  exit 1
end
