module Controller where

import Servant

import Api
import Types
import Models

import Utils.Controller
import Utils.ExtensibleRecords

import Controllers.Auth
import Controllers.Contest
import Controllers.Problem
import Controllers.Submit
import Controllers.Time
import Controllers.User

controller :: ServerT API (HandlerT IO)
controller = resourceController
