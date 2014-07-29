cmake_minimum_required(VERSION 2.8)
project(LAUNCHER)

if(CMAKE_HOST_WIN32)
    set(CABAL_BIN_DIR $ENV{APPDATA}/cabal/bin)
else()
    set(CABAL_BIN_DIR $ENV{HOME}/.cabal/bin)
endif()
find_program(CABAL cabal PATHS ${CABAL_BIN_DIR} NO_DEFAULT_PATH)
if(NOT CABAL)
  message(FATAL_ERROR "Could not find updated cabal! Please run 'cabal install cabal-install'")
endif()
message(STATUS "Found cabal at ${CABAL}")

set(LINKBOT_LABS_EXE_NAME "LinkbotLabs")
set(LINKBOT_LABS_MAIN_FILE "launcher.hs")
configure_file(launcher.cabal.in launcher.cabal @ONLY)

set(CFG_BROWSER_EXECUTABLE_PATH ${BROWSER_EXECUTABLE_PATH})
set(CFG_CONTENT_PATH "${CONTENT_PATH}")
set(CFG_PORT "0")
configure_file(LauncherConfig.hs.in LauncherConfig.hs @ONLY)

execute_process(COMMAND ${CABAL} sandbox init 
                WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

set(CABAL_INSTALL_OPTS -j)
file(COPY Setup.hs DESTINATION .)

if(WIN32)
  # Tell cabal to tell ghc to tell the linker to generate a GUI app.
  # Also use $ncpus (-j)
  # Also, tell cabal to tell configure to tell gcc to build for 32-bit
  # Also, do some thing that makes network happy re: configure
  set(CABAL_INSTALL_OPTS ${CABAL_INSTALL_OPTS} --ghc-options=-optl-mwindows --configure-option -host=i386-unknown-mingw32)
  file(COPY win32/icon.rc win32/linkbot-L-256x256.ico DESTINATION .)
endif()

add_custom_target(launcher ALL
  ${CABAL} install
    ${CABAL_INSTALL_OPTS}
    --extra-lib-dirs "${PROJECT_BINARY_DIR}"
    "${PROJECT_BINARY_DIR}"
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

install(PROGRAMS "${PROJECT_BINARY_DIR}/.cabal-sandbox/bin/${LINKBOT_LABS_EXE_NAME}${CMAKE_EXECUTABLE_SUFFIX}" 
        DESTINATION .)
