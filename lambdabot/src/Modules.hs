{-# LANGUAGE TemplateHaskell #-}

module Modules (modulesInfo) where

import Lambdabot.Main

-- to add a new plugin, one must first add a qualified import here, and also
-- add a string in the list below
import Lambdabot.Plugin.DSG
import Lambdabot.Plugin.IRC
import Lambdabot.Plugin.Social

modulesInfo :: Modules
modulesInfo = $(modules $ corePlugins
    ++ dsgPlugins
    ++ ["irc", "localtime", "topic"] -- ircPlugins
    ++ ["activity", "seen", "tell"] --socialPlugins
    )
