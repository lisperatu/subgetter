module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Subgetter.Core

main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "sublist.glade"
  window <- builderGetObject builder castToWindow "main"
  chooser <- builderGetObject builder castToFileChooserDialog "main"
  results <- builderGetObject builder castToTreeView "results"
  
  treeViewSetHeadersVisible results True
  
  colLang <- treeViewColumnNew
  colRelease <- treeViewColumnNew
  colMovie <- treeViewColumnNew

  treeViewColumnSetTitle colLang "Language"
  treeViewColumnSetTitle colRelease "Release name"
  treeViewColumnSetTitle colMovie "Movie name"
  
  treeViewColumnSetResizable colLang True
  treeViewColumnSetResizable colRelease True
  treeViewColumnSetResizable colMovie True

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew
  renderer3 <- cellRendererTextNew

  cellLayoutPackStart colLang renderer1 True
  cellLayoutPackStart colRelease renderer2 True
  cellLayoutPackStart colMovie renderer3 True
  
  treeViewAppendColumn results colLang
  treeViewAppendColumn results colRelease
  treeViewAppendColumn results colMovie

  
  onFileActivated chooser $ do
    Just filename <- fileChooserGetFilename chooser
    -- dialog <- messageDialogNew (Just window) [DialogDestroyWithParent] MessageInfo ButtonsNone filename
    -- _ <- dialogRun dialog
    url <- getSubsUrl filename
    doc <- openDoc url
    let subs = getSubs doc
    model <- listStoreNew subs
    cellLayoutSetAttributes colLang renderer1 model $ \row -> [ cellText := subtitleLang row ]
    cellLayoutSetAttributes colRelease renderer2 model $ \row -> [ cellText := releaseName row ]
    cellLayoutSetAttributes colMovie renderer3 model $ \row -> [ cellText := movieName row ]
    treeViewSetModel results model
    manager <- recentManagerGetDefault 
    Just folder <- fileChooserGetCurrentFolder chooser
    recentManagerAddItem manager $ "file://" ++ folder ++ "/"
    return ()
    
  onRowActivated results $ do
    return ()
  
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
