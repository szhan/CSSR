cmake_minimum_required(VERSION 3.3)
project(cpp_logging_recovery)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    cpp/AllStates.cpp
    cpp/AllStates.h
    cpp/Common.h
    cpp/G_Array.cpp
    cpp/G_Array.h
    cpp/Hash.cpp
    cpp/Hash.h
    cpp/Machine.cpp
    cpp/Machine.h
    cpp/Main.cpp
    cpp/Main.h
    cpp/ParseTree.cpp
    cpp/ParseTree.h
    cpp/States.cpp
    cpp/States.h
    cpp/SymbolToIndexMap.cpp
    cpp/SymbolToIndexMap.h
    cpp/Test.cpp
    cpp/Test.h
    cpp/TransTable.cpp
    cpp/TransTable.h)

add_executable(cpp_logging_recovery ${SOURCE_FILES})