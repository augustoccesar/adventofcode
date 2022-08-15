cmake_minimum_required(VERSION 3.13)
project(aoc)

set(CMAKE_CXX_STANDARD 20)

add_executable(
        aoc
        main.cpp

        Task.h
        String.h
        String.cpp

        day01/day01.cpp
        day02/day02.cpp
        day03/day03.cpp
        day04/day04.cpp
        day05/day05.cpp
        day07/day07.cpp
        day08/day08.h
        day08/day08.cpp
        day09/day09.cpp
        day10/day10.cpp
#SETUP:target_executable
)

add_custom_command(TARGET aoc PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
        ${CMAKE_SOURCE_DIR}/inputs/ $<TARGET_FILE_DIR:aoc>/inputs)

