module Controller where

import Servant

import Api
import Types
import Models

import Utils.Controller
import Utils.ExtensibleRecords

import Controllers.Time
import Controllers.Contest
import Controllers.User
import Controllers.Problem
import Controllers.Submit

controller :: ServerT API (HandlerT IO)
controller = resourceController
