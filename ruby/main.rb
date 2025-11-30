# frozen_string_literal: true

require 'dry/cli'
require_relative 'day'

module AOC
  module CLI
    module Commands
      extend Dry::CLI::Registry

      class Run < Dry::CLI::Command
        argument :year
        argument :day

        def call(year:, day:, **)
          year_int = year.to_i
          day_int = day.to_i

          if year_int.zero?
            puts 'Invalid value for year'
            exit 1
          end

          if day_int.zero?
            puts 'Invalid value for day'
            exit 1
          end

          day_instance = get_day(year_int, day_int)
          if day_instance.nil?
            puts "Day #{day_int} not found for year #{year_int}"
            exit 1
          end

          puts day_instance.part_one
          puts day_instance.part_two
        end
      end

      class Days < Dry::CLI::Command
        def call(**)
          get_days_registry.keys
            .map { |key| key.split("-") }
            .group_by(&:first)
            .transform_values { |entries| entries.map(&:last) }
            .each { |year, days| puts "#{year};#{days.join(";")}"}
        end
      end

      register 'days', Days
      register 'run', Run
    end
  end
end

Dry::CLI.new(AOC::CLI::Commands).call
