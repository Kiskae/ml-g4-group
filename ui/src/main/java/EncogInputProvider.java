/**
 * Created by Jasper on 21-12-2015.
 */

import agent.AlwaysLeftInputProvider;
import agent.PlayerInput;
import org.encog.ml.MLRegression;
import org.encog.ml.data.basic.BasicMLData;
import server.GameProperties;
import server.GameStateInterface;
import server.PhysicsProperties;
import java.util.Arrays;

public class EncogInputProvider extends AlwaysLeftInputProvider {

    private MLRegression _method;
    private GameProperties _gameProperties;
    private PhysicsProperties _physicsProperties;
    private PlayerInput _input = new PlayerInput();
    private int counter = 0;
    private int inputCounter = 0;

    public EncogInputProvider(MLRegression method, GameProperties gameProperties, PhysicsProperties physicsProperties) {
        this._method = method;
        this._gameProperties = gameProperties;
        this._physicsProperties = physicsProperties;
    }

    @Override
    public PlayerInput getInput(GameStateInterface state) {
                            // -1 .. 0    * 2  = -2 .. 0  +1 = -1 ... 1
        // use subsampling
        if(counter > 4 || counter == 0) {
            counter = 1;
//            // get the inputs of the network and scale them from ~ -1 to 1.
//            double ballX = (state.getBall().getPosX()) / (double)_gameProperties.sideWidth;
//            double ballY = (state.getBall().getPosY() / (double)(2*_gameProperties.sideWidth)) * 2.0 - 1.0;
//            double myX = (state.getMe().getPosX() / (double)_gameProperties.sideWidth) * 2.0 + 1.0;
//            double myY = (state.getMe().getPosY() / (double)(2*_gameProperties.sideWidth)) *2.0 -1.0;
////            double myVX = state.getMe().getVelX() / (double)_physicsProperties.playerHorizontalSpeed;
////            double myVY = state.getMe().getVelY() / (double)_physicsProperties.playerJumpVelocity;
////
////          // TODO: check scale of ballVX and ballVY
//            double ballVX = state.getBall().getVelY() / (double)(_physicsProperties.playerJumpVelocity * 3);
//            double ballVY = state.getBall().getVelY() / (double)(_physicsProperties.playerJumpVelocity * 3);
//
//            double oppX = state.getOpponent().getPosX() / (double)(_gameProperties.sideWidth);
//            double oppY = state.getOpponent().getPosY() / (double)(_gameProperties.sideWidth);
//            double oppVX = state.getOpponent().getVelX() / (double)_physicsProperties.playerHorizontalSpeed;
//            double oppVY = state.getOpponent().getVelY() / (double)_physicsProperties.playerJumpVelocity;

            // put them in an array and convert them to a MLData structure

            double ballXMinusOneToOne = state.getBall().getPosX() / (double)(_gameProperties.sideWidth);
            double ballYMinusOneToOne = state.getBall().getPosY() / (double)(2 * _gameProperties.sideWidth);

            double myXMinusOneToZero = state.getMe().getPosX() / (double)(_gameProperties.sideWidth);
            double myYMinusOneToOne = state.getMe().getPosY() / (double)(2 * _gameProperties.sideWidth);

            double oppXZeroToOne = state.getOpponent().getPosX() / (double)(_gameProperties.sideWidth);
            double oppYMinusOneToOne = (state.getOpponent().getPosY() - _gameProperties.sideWidth) / (double)(_gameProperties.sideWidth);


            double ballVXMinusOneToOne = state.getBall().getVelX() / (double)(_physicsProperties.playerJumpVelocity * 3);
            double ballVYMinusOneToOne = state.getBall().getVelY() / (double)(_physicsProperties.playerJumpVelocity * 3);
//
              double myVXMinusOneToOne = state.getMe().getVelX() / (double)_physicsProperties.playerHorizontalSpeed;
            double myVYMinusOneToOne = state.getMe().getVelY() / (double)_physicsProperties.playerJumpVelocity;
//            double ballXZeroToOne = (state.getBall().getPosX()+_gameProperties.sideWidth) / (double)(_gameProperties.sideWidth *2);
//            double ballYZeroToOne = state.getBall().getPosY() / (double)(800000);
//
//            double myXZeroToOne = (state.getMe().getPosX() +_gameProperties.sideWidth) / (double)(_gameProperties.sideWidth *2);
//            double myYZeroToOne = state.getMe().getPosY() / (double)(800000);
//
//            double ballVXZeroToOne = (state.getBall().getVelY() / (double)(_physicsProperties.playerJumpVelocity * 3) + 1.0) /2;
//            double ballVYZeroToOne = (state.getBall().getVelY() / (double)(_physicsProperties.playerJumpVelocity * 3) + 1.0) /2;


           // double posBallGrid[] = posAsGrid(ballXZeroToOne, ballYZeroToOne, myXZeroToOne, myYZeroToOne,  40);
            //double posMyGrid[] = posAsGrid(myXZeroToOne, myYZeroToOne, 20);

//            // make discrete (40 steps:):
//            double ballX = Math.round(ballXMinusOneToOne * 40.0) /40.0;
//            double ballY = Math.round(ballYMinusOneToOne *40.0) /40.0;
//            double myX = Math.round(myXMinusOneToZero * 40.0) /40.0;
//            double myY = Math.round(myYMinusOneToOne * 40.0) /40.0;
//            double ballVX = Math.round(ballVXMinusOneToOne * 40.0) /40.0;
//            double ballVY = Math.round(ballVYMinusOneToOne * 40.0) /40.0;

           //final double inputsArray[] = {state.getBall().getPosX(), state.getMe().getPosY(),state.getBall().getPosX(), state.getBall().getPosY()};
            final double inputsArray[] = {ballXMinusOneToOne, ballYMinusOneToOne, myXMinusOneToZero, myYMinusOneToOne, ballVXMinusOneToOne, ballVYMinusOneToOne};
           // BasicMLData inputs = new BasicMLData(combine(posBallGrid, null));
            // BasicMLData inputs = new BasicMLData(posBallGrid);
            //final double inputsArray[] = {ballXZeroToOne, ballYZeroToOne, myXZeroToOne, myYZeroToOne, ballVXZeroToOne, ballVYZeroToOne};
            BasicMLData inputs = new BasicMLData(inputsArray);


            // calculate the output of the network
            BasicMLData outputs = (BasicMLData)_method.compute(inputs);

            findBestAction(outputs.getData());

//            // set the controls
//            // left control
//            if(outputs.getData(0) < 0.5) {
//                _input.left = false;
//            } else {
//                this.inputCounter += 1;
//                _input.left = true;
//            }
//
//            // right control
//            if(outputs.getData(1) < 0.5) {
//                _input.right = false;
//            } else {
//                this.inputCounter += 1;
//                _input.right = true;
//            }
//
//            // up control
//            if(outputs.getData(2) < 0.5) {
//                _input.up = false;
//            } else {
//                this.inputCounter += 1;
//                _input.up = true;
//            }

//            // do nothing control
//            if(outputs.getData(3) < 0.5) {
//                _input.up = false;
//                _input.left = false;
//                _input.right = false;
//            }

            return _input;
        } else {
            counter++;
            return _input;
        }
    }

    private void findBestAction(double[] outputs) {
        int maxIndex = 0;
        double maxAsDouble = 0;
        for (int i = 0; i < outputs.length; i++) {
            if(outputs[i] > maxAsDouble) {
                maxAsDouble = outputs[i];
                maxIndex = i;
            }
        }

        _input.left = false;
        _input.right = false;
        _input.up = false;

        switch (maxIndex) {
            case 1:  _input.left = true;
                break;
            case 2:  _input.right = true;
                break;
            case 3:  _input.up = true;
                break;
            case 4:
                _input.up = true;
                _input.left = true;
                break;
            case 5:
                _input.up = true;
                _input.right = true;
                break;
            default:
                break;

        }
    }

    private double[] combine(double[] a, double[] b){
        int length = a.length + b.length;
        double[] result = new double[length];
        System.arraycopy(a, 0, result, 0, a.length);
        System.arraycopy(b, 0, result, a.length, b.length);
        return result;
    }

    private double[] posAsGrid(long x, long y, long maxX, long maxY, int gridWidthAndHeight) {

        double result[] = new double[gridWidthAndHeight*2];
        result[(int)Math.ceil(((double)x/(double)maxX * gridWidthAndHeight)-1)] = 1;
        result[(int)Math.ceil(((double)y/(double)maxY * gridWidthAndHeight)-1) + gridWidthAndHeight] = 1;
        return result;
    }


    private double[] posAsGrid(double x1, double y1, double x2, double y2, int gridWidthAndHeight) {

        double result[] = new double[gridWidthAndHeight*gridWidthAndHeight];

        double binSize = 1.0 / gridWidthAndHeight;
        double posX1 = Math.ceil(x1 / binSize);
        posX1 -= 1.0;
        posX1 = Math.max(posX1, 0);

        double posY1 = Math.ceil(y1 / binSize);
        posY1 -= 1.0;
        posY1 = Math.max(posY1, 0);

        int posInGrid1 = (int)((posY1 * gridWidthAndHeight) + posX1);



        result[posInGrid1] = 1;

        double posX2 = Math.ceil(x2 / binSize);
        posX2 -= 1.0;
        posX2 = Math.max(posX2, 0);

        double posY2 = Math.ceil(y2 / binSize);
        posY2 -= 1.0;
        posY2 = Math.max(posY2, 0);

        int posInGrid2 = (int)((posY2 * gridWidthAndHeight) + posX2);

        result[posInGrid2] = .5;

        return result;
    }

    public int getInputCounter() {
        return inputCounter;
    }
}
