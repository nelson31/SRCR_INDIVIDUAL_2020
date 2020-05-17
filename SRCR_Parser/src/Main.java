import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**         *********Sistemas de Representacao de Conhecimento e Raciocinio*********
 * Classe Main por onde se dá início à execução de todo o programa
 * @author Nelson Faria(A84727)
 * @version 1.0
 */
public class Main {

    /**
     * Método que é primeiramente chamada aquando do inicio da execução
     * do programa
     * @param args
     */
    public static void main(String[] args) {

        /** Leitura dos ficheiros Excel **/
        LeituraExcel le = new LeituraExcel();

        // Ler o ficheiro de Paragens de Oeiras
        le.leituraFileParagens();
        // Ler o ficheiro de Adjacencias
        le.leituraFileAdjacencias();
        // Listas com os dados retirados da leitura
        List<Paragem> paragens = le.getParagens();
        List<Carreira> adjacencias = le.getAdjacencias();

        /** Escrita dos ficheiros da base de Conhecimento **/
        EscritaProlog ep = new EscritaProlog(le.getValoresDesconhecidosP(),le.getValoresDesconhecidosA(), paragens, adjacencias);

        // Escrita dos dados da paragem num ficheiro que representa a base de Conhecimento respetiva
        ep.escreveParagens();
        // Escrita dos dados das adjacencias num ficheiro que representa a base de Conhecimento respetiva
        ep.escreveAdjacencias();
    }
}
