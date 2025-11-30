# frozen_string_literal: true

class Day
  attr_reader :year, :day

  def initialize(year, day)
    @year = year
    @day = day
  end

  def self.inherited(subclass)
    super
    @descendants ||= []
    @descendants << subclass
  end

  def self.descendants
    @descendants ||= []
  end

  def part_one
    raise NotImplementedError, 'part_one must be implemented'
  end

  def part_two
    raise NotImplementedError, 'part_two must be implemented'
  end

  def read_input(suffix = '')
    path = if suffix.empty?
             "../inputs/#{@year}_#{@day.to_s.rjust(2, '0')}.txt"
           else
             "../inputs/#{@year}_#{@day.to_s.rjust(2, '0')}_#{suffix}.txt"
           end

    File.read(path)
  rescue Errno::ENOENT
    raise "Input file not found: #{path}"
  end
end

# rubocop:disable Style/MutableConstant
DAY_REGISTRY = {}
# rubocop:enable Style/MutableConstant

def load_days
  base_dir = File.dirname(__FILE__)
  year_dirs = Dir.glob(File.join(base_dir, 'y*')).select { |d| File.directory?(d) }

  year_dirs.each do |year_dir|
    day_files = Dir.glob(File.join(year_dir, 'd*.rb'))

    day_files.each do |day_file|
      year = File.basename(year_dir).gsub('y', '').to_i
      day = File.basename(day_file, '.rb').gsub('d', '').to_i

      classes_before = Day.descendants.dup

      require_relative File.join(File.basename(year_dir), File.basename(day_file, '.rb'))

      classes_after = Day.descendants
      new_classes = classes_after - classes_before

      new_classes.each do |day_class|
        bound_class = Class.new(day_class) do
          define_method :initialize do
            super(year, day)
          end
        end

        DAY_REGISTRY["#{year}-#{day}"] = bound_class
      end
    rescue LoadError => e
      warn "Failed to load #{day_file}: #{e.message}"
    rescue StandardError => e
      warn "Error processing #{day_file}: #{e.message}"
    end
  end
end

def get_days_registry
  load_days if DAY_REGISTRY.empty?

  DAY_REGISTRY
end

def get_day(year, day)
  load_days if DAY_REGISTRY.empty?

  day_class = DAY_REGISTRY["#{year}-#{day}"]
  return nil unless day_class

  day_class.new
end
