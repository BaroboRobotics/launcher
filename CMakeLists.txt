cmake_minimum_required(VERSION 2.8)

project(launcher NONE)
set(PROJECT_VERSION "0.0.0")

#
# CONFIGURE
#

# Set/check for config variables
if(CMAKE_HOST_WIN32 AND NOT DEFINED GHC_ROOT)
  if(DEFINED ENV{GHC_ROOT})
    set(GHC_ROOT "$ENV{GHC_ROOT}")
  else()
    message(FATAL_ERROR "The Linkbot Labs launcher requires GHC's root path in GHC_ROOT")
  endif()
endif()

if(CMAKE_HOST_WIN32)
  set(LAUNCHER_BUILD_PATH "${GHC_ROOT}/bin;${GHC_ROOT}/mingw/bin;$ENV{PATH}")
else()
  set(LAUNCHER_BUILD_PATH $ENV{PATH})
endif()

set(LINKBOT_LABS_EXE_NAME "LinkbotLabs")
set(LINKBOT_LABS_MAIN_FILE "launcher.hs")

set(CFG_BROWSER_EXECUTABLE_PATH ${BROWSER_EXECUTABLE_PATH})
set(CFG_CONTENT_PATH "${CONTENT_PATH}")
set(CFG_PORT "0")

set(CABAL_SANDBOX "cabal-sandbox")

# Require cabal
if(CMAKE_HOST_WIN32)
    set(CABAL_BIN_DIR $ENV{APPDATA}/cabal/bin)
else()
    set(CABAL_BIN_DIR $ENV{HOME}/.cabal/bin)
endif()
find_program(CABAL cabal PATHS ${CABAL_BIN_DIR} NO_DEFAULT_PATH)
if(NOT CABAL)
  message(FATAL_ERROR "Could not find updated cabal! Please run 'cabal install cabal-install'")
endif()

# Require ghc
find_program(GHC ghc PATHS ${LAUNCHER_BUILD_PATH} NO_DEFAULT_PATH)
if(NOT GHC)
  message(FATAL_ERROR "Oh dogg where is GHC?")
endif()

# Copy configuration files to build dir
configure_file(launcher.cabal.in launcher.cabal @ONLY)
configure_file(LauncherConfig.hs.in LauncherConfig.hs @ONLY)
file(COPY Setup.hs DESTINATION .)
if(WIN32)
  file(COPY win32/icon.rc win32/linkbot-L-256x256.ico DESTINATION .)
endif()
file(WRITE ${PROJECT_BINARY_DIR}/build.sh
"set -e

winpath () {
  if uname -a | grep -qi msys; then
    echo \"$1\" | sed -e 's#;#:#g; s#\\(^\\|:\\)\\([A-Z]\\):#\\1/\\2#ig; s#\\\\#/#g'
  else
    echo \"$1\"
  fi
}

export PATH=\"$(winpath '@LAUNCHER_BUILD_PATH@')\"

@CABAL@ sandbox --sandbox='@CABAL_SANDBOX@' init
@CABAL@ install -j"
)

#
# DEFINE BUILD RULES
#

add_custom_target(RUN_CABAL ALL "sh" "build.sh")

#
# DEFINE INSTALL RULES
#

install(PROGRAMS
  "${PROJECT_BINARY_DIR}/${CABAL_SANDBOX}/bin/${LINKBOT_LABS_EXE_NAME}${CMAKE_EXECUTABLE_SUFFIX}"
  DESTINATION .)
