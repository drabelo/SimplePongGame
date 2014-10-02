import cmpsci220._
import cmpsci220.graphics._
import cmpsci220.hw.pong._

// Assume that width > 0 and height > 0
case class Table(width: Int, height: Int)

// (x,y) is the bottom coordinate and (x, y + height) is the top coordinate
case class Paddle(pos: Vector2D, height: Double)

//Ball
case class Ball(radius: Double, position: Vector2D, velocity: Vector2D)

case class Game(table: Table,
                paddle1: Paddle,
                paddle2: Paddle,
                velocity1: Vector2D,
                velocity2: Vector2D,
                ball: Ball)  

val initGame = Game(Table(900,500),
	 				Paddle(Vector2D(0,150), 60),
	 				Paddle(Vector2D(0,150), 60),
	 				Vector2D(0.0,0.0),
	 				Vector2D(0.0,0.0),
	 				Ball(20, Vector2D(250, 150), Vector2D(10,7))
					 )

test("Testing is valid Paddle - should be false") {
  assert(isValidPaddle(Paddle(Vector2D(2000,2000), 60), Table(500,500))
  	== false)
}

test("Testing is valid Paddle - should be true") {
  assert(isValidPaddle(Paddle(Vector2D(300,200), 60), Table(500,500))
  	== true)
}


def isValidPaddle(paddle: Paddle, table: Table): Boolean = 
{
	if(paddle.pos.x >= 0 && paddle.pos.x <= table.width)
	{
		if(paddle.pos.y >= 0 && paddle.pos.y <= table.height)
		{
		true
		}else
		{
			false
		}

	}
	else{
		false
	}
}

test("Testing moving the paddle") {
  assert(movePaddle(initGame.paddle1, initGame.table, Vector2D(0,10)).get
  	== Paddle(Vector2D(0, 160), initGame.paddle1.height))
}

def movePaddle(paddle: Paddle, table: Table, velocity: Vector2D): Option[Paddle] = {
  val movedPaddle = paddle.copy(pos = paddle.pos + velocity)
  if (isValidPaddle(movedPaddle, table)) 
  {
  	
    Some(movedPaddle)
  }
  else 
  {
     None
  }
}


def keyPressed(key: String, game: Game): Game = key match {
  case "a" =>game.copy(velocity1 = Vector2D(0.0,20.0))
  case "z" =>game.copy(velocity1 = Vector2D(0.0,-20.0))
  case "k" => game.copy(velocity2 = Vector2D(0.0,20.0))
  case "m" =>game.copy(velocity2 = Vector2D(0.0,-20.0))
  case _ => game  // ignore the key
}


def keyReleased(key: String, game: Game): Game = key match {
  case "a" =>game.copy(velocity1 = Vector2D(0.0,0.0))
  case "z" =>game.copy(velocity1 = Vector2D(0.0,0.0))
  case "k" => game.copy(velocity2 = Vector2D(0.0,0.0))
  case "m" =>game.copy(velocity2 = Vector2D(0.0,0.0))
  case _ => game  // ignore the key
}


test("Testing moving both paddles -- should be true") {

  assert(moveBothPaddles(Game(Table(900,500),
	 					Paddle(Vector2D(0,150), 60),
	 					Paddle(Vector2D(0,150), 60),
	 					Vector2D(0.0,10.0),
	 					Vector2D(0.0,10.0),
	 					Ball(20, Vector2D(250, 150), Vector2D(10,7))
					)) ==

					 Game(Table(900,500),
	 				Paddle(Vector2D(0,160), 60),
	 				Paddle(Vector2D(0,160), 60),
	 				Vector2D(0.0,10.0),
	 				Vector2D(0.0,10.0),
	 				Ball(20, Vector2D(250, 150), Vector2D(10,7))
					 ))
}


def moveBothPaddles(game: Game): Game = {
	val container:Array[Paddle] = new Array[Paddle](2)

	movePaddle(game.paddle2, game.table, game.velocity2) match 
	{
  		case None => container(0) = game.paddle2
		case Some(newPaddle: Paddle) => container(0) = newPaddle
	}

	movePaddle(game.paddle1, game.table, game.velocity1) match 
	{
  		case None => container(1) = game.paddle1
 		case Some(newPaddle: Paddle) => container(1) = newPaddle
	}
	game.copy(paddle2 = container(0), paddle1 = container(1))
}

def drawGame(game: Game): Image = 
{
	val table = graphics.fillRect(game.table.width, game.table.height, black)
	val player1 = move(graphics.fillRect(5, game.paddle1.height, graphics.rgb(1,1,1)),0, game.paddle1.pos.y)
	val player2 = move(graphics.fillRect(5, game.paddle2.height, graphics.rgb(1,1,1)),(game.table.width-10), game.paddle2.pos.y)
	val ball = move(graphics.solidOval(game.ball.radius, game.ball.radius, graphics.rgb(1,1,1)), game.ball.position.x, game.ball.position.y )

	val step1 = graphics.overlay(player1, table)
	val step2 = graphics.overlay(player2, step1)
	val step3 = graphics.overlay(ball, step2)
	step3
}


test("Testing hitting the left side of the table") {
  assert(hasHitTable(Ball(15, Vector2D(0,150), Vector2D(-5,5)), initGame.table)
  	== true)
}

test("Testing hitting the right side of the table") {
  assert(hasHitTable(Ball(15, Vector2D(initGame.table.width,150), Vector2D(5,5)), initGame.table)
  	== true)
}

test("Testing hitting the top side of the table") {
  assert(hasHitTable(Ball(15, Vector2D(200,initGame.table.height), Vector2D(5,5)), initGame.table)
  	== true)
}

test("Testing hitting the bottom side of the table") {
  assert(hasHitTable(Ball(15, Vector2D(initGame.table.width,0), Vector2D(5,5)), initGame.table)
  	== true)
}

def hasHitTable(ball: Ball, table: Table): Boolean = 
{
	if(ball.position.x <= 0)
		{true}
	else if(ball.position.x >= table.width)
		{true}
	else if(ball.position.y <= 0)
		{true}
	else if(ball.position.y >= table.height)
		{true}
	else
	{
	false
	}	
}

test("Testing hitting paddle number 1") {
  assert(hasHitPaddle(Ball(15, Vector2D(0,150), Vector2D(5,5)), initGame.paddle1)
  	== true)
}

test("Testing hitting paddle number 2") {
  assert(hasHitPaddle(Ball(15, Vector2D(initGame.table.width, initGame.paddle1.pos.y), Vector2D(5,5)), initGame.paddle2)
  	== true)
}

def hasHitPaddle(ball: Ball, paddle: Paddle): Boolean = 
{
	if(ball.position.distanceToLineSegment(Vector2D(0,paddle.pos.y), Vector2D(0,(paddle.pos.y + paddle.height))) <= ball.radius){
		true
	}
	else if(ball.position.distanceToLineSegment(Vector2D(initGame.table.width , paddle.pos.y), Vector2D(initGame.table.width,(paddle.pos.y + paddle.height))) <= ball.radius)
	{
		true
	}
	else{
		false
	}
	
}

test("Moving the ball using vector") {
	assert(moveBall(				Game(Table(900,500),
	 					Paddle(Vector2D(0,150), 60),
	 					Paddle(Vector2D(0,150), 60),
	 					Vector2D(0.0,10.0),
	 					Vector2D(0.0,10.0),
	 					Ball(20, Vector2D(250, 150), Vector2D(10,7))))


		== 				Game(Table(900,500),
	 					Paddle(Vector2D(0,150), 60),
	 					Paddle(Vector2D(0,150), 60),
	 					Vector2D(0.0,10.0),
	 					Vector2D(0.0,10.0),
	 					Ball(20, Vector2D(260, 157), Vector2D(10,7))
	 					)
		)


}

def moveBall(game: Game): Game =
{
	//if everything is right == move ball
	if(hasHitTable(game.ball, game.table) == false && hasHitPaddle(game.ball, game.paddle1) == false &&
														hasHitPaddle(game.ball,game.paddle2) == false)
	{
			val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + game.ball.velocity),
										   	game.ball.velocity
											))

		return updatedGame
	}

	else if(hasHitPaddle(game.ball, game.paddle1) == true)
	{
				val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position),
										   	Vector2D( (-1 * game.ball.velocity.x),
										   		game.ball.velocity.y)
											))

				val updatedGame1 = updatedGame.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + updatedGame.ball.velocity),
										   	updatedGame.ball.velocity
											))
				return updatedGame1

	}
	else if(hasHitPaddle(game.ball, game.paddle2) == true){
			val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position),
										   	Vector2D( (-1 * game.ball.velocity.x),
										   		game.ball.velocity.y)
											))

			val updatedGame1 = updatedGame.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + updatedGame.ball.velocity),
										   	updatedGame.ball.velocity
											))
			return updatedGame1

	}

	else {
		if(game.ball.position.x <= 0)
		{
			// -x and y
			if(game.ball.position.y >= game.table.height){
				val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position),
										   	Vector2D( (-1 * game.ball.velocity.x),
										   		(-1 * game.ball.velocity.y)
											)))

				val updatedGame1 = updatedGame.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + updatedGame.ball.velocity),
										   	updatedGame.ball.velocity
											))
				return updatedGame1
			}
			else{
						val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position),
										   	Vector2D( (-1 * game.ball.velocity.x),
										   		game.ball.velocity.y)
											))

						val updatedGame1 = updatedGame.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + updatedGame.ball.velocity),
										   	updatedGame.ball.velocity
											))
						return updatedGame1

			}
				game
			

		}
		else if(game.ball.position.x >= game.table.width)
		{
			//-x and y
				if(game.ball.position.y <= 0){
				val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position),
										   	Vector2D( (-1 * game.ball.velocity.x),
										   		(-1 * game.ball.velocity.y)
											)))

				val updatedGame1 = updatedGame.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + updatedGame.ball.velocity),
										   	updatedGame.ball.velocity
											))
				return updatedGame1
		}
			else
			{
						val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position),
										   	Vector2D( (-1 * game.ball.velocity.x),
										   		game.ball.velocity.y)
											))

						val updatedGame1 = updatedGame.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + updatedGame.ball.velocity),
										   	updatedGame.ball.velocity
											))
						return updatedGame1

			}
		}
		else if(game.ball.position.y <= 0)
		{
			// x and -y
			val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position),
										   	Vector2D( game.ball.velocity.x,
										   		(-1 * game.ball.velocity.y))
											))

			val updatedGame1 = updatedGame.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + updatedGame.ball.velocity),
										   	updatedGame.ball.velocity)
											)

			return updatedGame1

		}
		else if(game.ball.position.y >= (game.table.height - game.ball.radius))
		{
			
			// x and -y
		val updatedGame = game.copy(ball = Ball(game.ball.radius,
										   (game.ball.position),
										   	Vector2D( game.ball.velocity.x,
										   		(-1 * game.ball.velocity.y))
											))
			val updatedGame1 = updatedGame.copy(ball = Ball(game.ball.radius,
										   (game.ball.position + updatedGame.ball.velocity),
										   	updatedGame.ball.velocity
											))
			return updatedGame1
		} else
		{
			game
		}
	}
}




animate(init = initGame,
        draw = drawGame,
        width = initGame.table.width,
        height = initGame.table.height,
        tick = (game: Game) => { moveBall(moveBothPaddles(game)) },
        keyPressed = keyPressed,
        keyReleased = keyReleased)