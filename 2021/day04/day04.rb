# frozen_string_literal: true

# TODO(augustoccesar)[2021-12-14]: Deal with integers from the start to avoid so many to_i's
# TODO(augustoccesar)[2021-12-14]: Explain what am I doing with some comments

require_relative "../task"

class Day04
  include Task

  def part_one
    numbers, boards = parse_input(read_input)
    marked_track = Array.new(boards.size) { [Array.new(5, 0), Array.new(5, 0)] }
    marked_track_nums = Array.new(boards.size) { [] }

    winner = nil
    while winner.nil?
      num = numbers.shift
      boards.each_with_index do |board, i|
        pos = board[num]
        next if pos.nil?

        marked_track[i][0][pos[0]] += 1
        marked_track[i][1][pos[1]] += 1
        marked_track_nums[i] << num

        winner = i if marked_track[i][0][pos[0]] == 5 || marked_track[i][1][pos[1]] == 5
      end
    end

    unmarked_sum = (boards[winner].keys.map(&:to_i) - marked_track_nums[winner].map(&:to_i)).reduce(&:+)
    last_marked = marked_track_nums[winner][-1].to_i

    (unmarked_sum * last_marked).to_s
  end

  def part_two
    "-"
  end

  private

  def parse_input(input)
    data = input.split("\n\n")
    numbers = data[0].split(",")
    boards_data = data[1..-1].map { |it| it.split("\n") }.map { |it| it.map(&:split) }

    board_maps = Array.new(boards_data.size) { {} }
    boards_data.each_with_index do |board, board_index|
      board.each_with_index do |row, row_index|
        row.each_with_index do |item, col_index|
          board_maps[board_index][item] = [row_index, col_index]
        end
      end
    end

    [numbers, board_maps]
  end
end
