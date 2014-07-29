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

# If we're on Windows, set the cabal command up so that it spawns with GHC's
# MinGW toolchain in front of its PATH.
if(NOT CMAKE_HOST_WIN32)
  set(CABAL_ESCAPED ${CABAL})
else()
  if(NOT DEFINED GHC_ROOT)
    if(DEFINED ENV{GHC_ROOT})
      set(GHC_ROOT "$ENV{GHC_ROOT}")
    else()
      message(FATAL_ERROR "The Linkbot Labs launcher requires GHC's root path in GHC_ROOT")
    endif()
  endif()

  find_package(CYGWIN)
  if(NOT CYGWIN_INSTALL_PATH)
    # If we're on 64-bit, but building for a 32-bit target, then our registry
    # view might be blinded (http://public.kitware.com/Bug/view.php?id=7095).
    find_path(CYGWIN_INSTALL_PATH cygwin.bat "C:/Cygwin64")
    if(NOT CYGWIN_INSTALL_PATH)
      message(FATAL_ERROR "The Linkbot Labs launcher requires Cygwin. Either "
        "install Cygwin in C:/Cygwin, C:/Cygwin64, or pass "
        "-DCYGWIN_INSTALL_PATH to CMake.")
    endif()
  endif()

  set(ghc_paths "$(cygpath -p \"${GHC_ROOT}/mingw/bin\;${GHC_ROOT}/bin\")")
  set(cygwin_paths "/usr/local/bin:/usr/bin")
  set(cabal_paths "PATH=${cygwin_paths}:$PATH\; PATH=${ghc_paths}:$PATH")

  # We need two version of cmd, one for executing with execute_process, and the
  # other Makefile-escaped for add_custom_target.
  set(cmd "${cabal_paths}\; \"$(cygpath '${CABAL}')\" \"$@\"")
  string(REPLACE "$" "$$" cmd_escaped "${cmd}")
  string(REPLACE "\"" "\\\"" cmd_escaped "${cmd_escaped}")

  # Run cabal in mintty to provide it a real Cygwin environment. If it is not
  # run this way, then cabal's ghc and setup.exe subprocesses sometimes fail
  # with "<stdout> hFlush: invalid argument (Bad file descriptor)".
  find_program(MINTTY mintty PATHS ${CYGWIN_INSTALL_PATH}/bin NO_DEFAULT_PATH)
  find_program(BASH bash PATHS ${CYGWIN_INSTALL_PATH}/bin NO_DEFAULT_PATH)
  set(MINTTY_OPTS)
  if(DEFINED ENV{TERM})
    # We're already in a UNIXy environment, so we should be able to just
    # redirect mintty's stdout with --log -. Note: if we're not, then stdout
    # will just get lost. Passing no options will make mintty spawn its own
    # window.
    set(MINTTY_OPTS --window hide --log -)
  endif()
  # FIXME little bug, maybe--$0 will be set to a Windows-style path within
  # Cygwin, by passing ${BASH} as the last argument.
  set(CABAL ${MINTTY} ${MINTTY_OPTS} ${BASH} -c "${cmd}" ${BASH})
  set(CABAL_ESCAPED ${MINTTY} ${MINTTY_OPTS} ${BASH} -c "\"${cmd_escaped}\"" ${BASH})
endif()

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
  ${CABAL_ESCAPED} install
    ${CABAL_INSTALL_OPTS}
    --extra-lib-dirs "${PROJECT_BINARY_DIR}"
    "${PROJECT_BINARY_DIR}"
  WORKING_DIRECTORY ${PROJECT_BINARY_DIR})

install(PROGRAMS "${PROJECT_BINARY_DIR}/.cabal-sandbox/bin/${LINKBOT_LABS_EXE_NAME}${CMAKE_EXECUTABLE_SUFFIX}" 
        DESTINATION .)
