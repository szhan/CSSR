cmake_minimum_required(VERSION 2.8.11)
project(CSSR)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wextra -Wall -O1 -std=c++11")

##------------------------------------------------------------------------------
## Required CPM Setup - no need to modify - See: https://github.com/iauns/cpm
##------------------------------------------------------------------------------
#set(CPM_DIR "${CMAKE_CURRENT_BINARY_DIR}/cpm_packages" CACHE TYPE STRING)
#find_package(Git)
#if(NOT GIT_FOUND)
#  message(FATAL_ERROR "CPM requires Git.")
#endif()
#if (NOT EXISTS ${CPM_DIR}/CPM.cmake)
#  message(STATUS "Cloning repo (https://github.com/iauns/cpm)")
#  execute_process(
#      COMMAND "${GIT_EXECUTABLE}" clone https://github.com/iauns/cpm ${CPM_DIR}
#      RESULT_VARIABLE error_code
#      OUTPUT_QUIET ERROR_QUIET)
#  if(error_code)
#    message(FATAL_ERROR "CPM failed to get the hash for HEAD")
#  endif()
#endif()
#include(${CPM_DIR}/CPM.cmake)
##------------------------------------------------------------------------------
## ++ MODULE: EasyLogging
#CPM_AddModule("easylogging++"
#    GIT_REPOSITORY "https://github.com/easylogging/easyloggingpp"
#    GIT_TAG "v9.80")
#
#CPM_Finish()
#------------------------------------------------------------------------------

set(CSSR_VERSION_MAJOR 0)
set(CSSR_VERSION_MINOR 1)
set(SOURCE_FILES
    Main.cpp
    Machine.cpp
    SymbolToIndexMap.cpp
    Hash.cpp
    G_Array.cpp
    ParseTree.cpp
    States.cpp
    AllStates.cpp
    Test.cpp
    TransTable.cpp
    Common.h)

add_executable(CSSR ${SOURCE_FILES})

#target_link_libraries(${CSSR_EXECUTABLE} ${CPM_LIBRARIES})

