module Topics
     ( myTopics
     , myTopicConfig
     , spawnEditorIn
     , spawnEditor
     , spawnFileIn
     , spawnFile
     , spawnShellIn
     , spawnShell
     , createOrGoto
     , createGoto
     , promptedGoto
     , promptedShift
     ) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Config
import Utils

myTopics :: [Topic]
myTopics = [ "dashboard"
           , "code", "web"
           , "music", "video", "file"
           , "pdf", "skype"
           ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs = M.fromList $
        [ ( "dashboard" , ""                        )
        , ( "code"      , "Code"                    )
        , ( "web"       , "Downloads"               )
        , ( "music"     , "Music"                   )
        , ( "video"     , "Video"                   )
        , ( "pdf"       , "Books"                   )
        , ( "file"      , ""                        )
        , ( "skype"     , ""                        )
        ]
    , defaultTopicAction = const $ spawn ""
    , defaultTopic       = "dashboard"
    , topicActions       = M.fromList $
        [ ( "code"      , spawnShell                ) -- Shell
        , ( "web"       , spawn "chromium"          ) -- Web browser
        , ( "music"     , spawn "termite -e ncmpcpp") -- Music player
        , ( "video"     , videoSelect               ) -- Video selection prompt
        , ( "pdf"       , pdfSelect                 ) -- PDF selection prompt
        , ( "file"      , spawn "termite -e ranger" ) -- File manager
        , ( "skype"     , spawn "skype"             ) -- Skype
        ]
    }

goto :: WorkspaceId -> X ()
goto = switchTopic myTopicConfig

shift :: WorkspaceId -> X ()
shift = windows . W.shift

spawnEditorIn :: Dir -> X ()
spawnEditorIn dir = spawn $ "termite -e vim " ++ dir

spawnEditor :: X ()
spawnEditor = currentTopicDir myTopicConfig >>= spawnEditorIn

spawnFileIn :: Dir -> X ()
spawnFileIn dir = spawn $ "termite -e ranger " ++ dir

spawnFile :: X ()
spawnFile = currentTopicDir myTopicConfig >>= spawnFileIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "termite -d " ++ dir

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ shift

createGoto :: WorkspaceId -> X ()
createGoto w = newWorkspace w >> switchTopic myTopicConfig w

createOrGoto :: WorkspaceId -> X ()
createOrGoto w = do
    exists <- workspaceExist w
    if (not exists)
    then createGoto w
    else goto w

newWorkspace :: WorkspaceId -> X ()
newWorkspace w = do
    exists <- workspaceExist w
    if (not exists)
    then addHiddenWorkspace w
    else return ()

workspaceExist :: WorkspaceId -> X Bool
workspaceExist w = do xs <- get
                      return $ workspaceExists w ( windowset xs )

workspaceExists :: WorkspaceId -> W.StackSet WorkspaceId l a s sd -> Bool
workspaceExists w ws = w `elem` map W.tag (W.workspaces ws)
