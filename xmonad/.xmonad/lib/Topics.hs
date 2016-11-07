module Topics where

import           XMonad
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.TopicSpace
import           XMonad.Prompt.Workspace
import           XMonad.Util.Run

import           Control.Monad
import           Data.Default                     (def)
import qualified Data.Map                         as M
import qualified XMonad.StackSet                  as W

import           Config
import           Utils

myTopics :: [Topic]
myTopics =
    [ "dashboard", "web", "speak"
    , "code", "doc", "pdf"
    , "music", "video", "file"
    , "NSP"
    ]

myTopicDirs = M.fromList
    [ ( "dashboard", "")
    , ( "note"     , "notes")
    , ( "code"     , "src")
    , ( "web"      , "downloads")
    , ( "music"    , "music")
    , ( "video"    , "video")
    , ( "pdf"      , "books")
    , ( "file"     , "")
    , ( "speak"    , "")
    , ( "doc"      , "documents")
    ]

myTopicActions = M.fromList
    [ -- Editor
      ( "note", spawnEditor)
      -- Terminal
    , ( "code", spawnTerm)
      -- Web browser
    , ( "web", spawn myBrowser)
      -- Music player
    , ( "music", runInTerm "-t music" "ncmpcpp")
      -- Video selection prompt
    , ( "video", videoSelect)
      -- PDF selection prompt
    , ( "pdf", spawn myPDFViewer)
      -- File manager
    , ( "file", spawnFile)
      -- IRC and E-mail
    , ( "speak", runInTerm "-t chat" "chat" >> runInTerm "-t mail" "mutt")
      -- Documents
    , ( "doc", spawn "libreoffice")
    ]

myTopicConfig :: TopicConfig
myTopicConfig = def
    { topicDirs = myTopicDirs
    , defaultTopicAction = const spawnTerm
    , defaultTopic = "dashboard"
    , topicActions = myTopicActions
    }

spawnEditorIn :: Dir -> X ()
spawnEditorIn dir = spawn $ myEditor ++ " " ++ dir

spawnEditor :: X ()
spawnEditor = currentTopicDir myTopicConfig >>= spawnEditorIn

spawnFileIn :: Dir -> X ()
spawnFileIn dir = runInTerm "-t file" ("ranger " ++ dir)

spawnFile :: X ()
spawnFile = currentTopicDir myTopicConfig >>= spawnFileIn

spawnTermIn :: Dir -> X ()
spawnTermIn dir = spawn $ myTerm ++ " -d " ++ dir

spawnTerm :: X ()
spawnTerm = currentTopicDir myTopicConfig >>= spawnTermIn

goto :: WorkspaceId -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

shift :: WorkspaceId -> X ()
shift = windows . W.shift

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig shift

createGoto :: WorkspaceId -> X ()
createGoto w = newWorkspace w >> switchTopic myTopicConfig w

createOrGoto :: WorkspaceId -> X ()
createOrGoto w = do
    exists <- workspaceExist w
    if not exists
    then createGoto w
    else goto w

newWorkspace :: WorkspaceId -> X ()
newWorkspace w = do
    exists <- workspaceExist w
    unless exists $ addHiddenWorkspace w

workspaceExist :: WorkspaceId -> X Bool
workspaceExist w = do
    xs <- get
    return $ workspaceExists w ( windowset xs )

workspaceExists :: WorkspaceId -> W.StackSet WorkspaceId l a s sd -> Bool
workspaceExists w ws = w `elem` map W.tag (W.workspaces ws)
