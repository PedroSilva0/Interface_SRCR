/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package pkginterface;
import java.util.ArrayList;
import se.sics.jasper.*;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author rcamposinhos
 */
public class Prolog {
    SICStus sp;

    public Prolog(String filepath) throws SPException{
        sp = new SICStus();
        sp.load(filepath);
    }
    
    //Initalize SICStus virtual machine
    public void loadSICStus() throws SPException {
        sp = new SICStus();
    }
    //Load SICStus script
    public void loadSICStusScript(String filepath) throws SPException {
        sp.load(filepath);
    }
    
    public ArrayList<String> query(String queryStr) throws Exception{
        ArrayList<String> result = new ArrayList<>();
        HashMap map = new HashMap();
        
        Query query = sp.openPrologQuery(queryStr, map);
        
        while(query.nextSolution()){
            result.add(map.toString());
        }
        
        query.close();
        
        
        return result;
    }

    
}
