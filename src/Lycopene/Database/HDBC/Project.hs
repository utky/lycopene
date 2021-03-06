{-# LANGUAGE GADTs #-}
module Lycopene.Database.HDBC.Project where

import           Lycopene.Database (Persist)
import           Lycopene.Core ( ProjectF(..)
                               , Project(..))

-- 2016-08-03
-- 進め方を整理
--
-- ドメインはいくつかのコマンドの合成で表現される
-- (Fluxのようにstateの塊を引き回すわけではない)
--
-- コマンドをHDBC操作へと変換した後実行する
-- これはドメインのセマンティクスを
-- HDBCの操作的意味論へと翻訳する作業を表す。
--
-- HDBCの操作をまずは定義する。
--
-- * どめいんをStatementにする
--     * Statementのprepareが都度発生する
-- * Statementを連結する
-- * Statementの出力をどめいんに戻す
--
-- まとめて最初にStatementを作っておくのは難しいな
--

-- TODO: 
persistProject :: ProjectF a -> Persist a
persistProject _ = undefined
