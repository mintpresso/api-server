package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._

import play.api.Play.current
import play.api.libs._
import play.api.cache._
import play.api.libs.json._

import models._
import anorm._ 

object Query extends Controller {
  
  def findFilters(accId: Long) = TODO
  def addFilter(accId: Long) = TODO
  def findOrders(accId: Long) = TODO
  def getOrder(accId: Long, orderId: Long) = TODO
  def addOrder(accId: Long) = TODO
  
}