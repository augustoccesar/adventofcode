# frozen_string_literal: true

module Task
  def read_input(suffix = "input")
    day = self.class.to_s.downcase
    file = File.open("#{Dir.pwd}/inputs/#{day}_#{suffix}.txt")
    data = file.read
    file.close

    data
  end
end
