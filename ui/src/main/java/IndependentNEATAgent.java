import agent.AlwaysLeftInputProvider;
import org.encog.ml.MLRegression;
import org.encog.neural.neat.NEATNetwork;
import server.GameProperties;
import server.PhysicsProperties;

import java.io.FileInputStream;
import java.io.ObjectInputStream;


/**
 * Created by Jasper on 19-1-2016.
 */
public class IndependentNEATAgent {

    public static AlwaysLeftInputProvider getAnAgent(String fileName) {
        NEATNetwork network = null;
        try {

            FileInputStream fin = new FileInputStream(fileName);
            ObjectInputStream ois = new ObjectInputStream(fin);
            network = (NEATNetwork) ois.readObject();
            ois.close();

        } catch (Exception e) {
            e.printStackTrace();
        }

        MLRegression methodAsMLRegression = (MLRegression) network;
        GameProperties gameProps = new GameProperties();
        PhysicsProperties physProps = new PhysicsProperties();
        AlwaysLeftInputProvider inputProvider = new EncogInputProvider(methodAsMLRegression, gameProps, physProps);

        return inputProvider;
    }
}
