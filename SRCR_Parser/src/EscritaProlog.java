import javax.print.DocFlavor;
import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**         *********Sistemas de Representacao de Conhecimento e Raciocinio*********
 * Classe EscritaProlog que serve para a partir dos objetos criados
 * aquando da leitura do ficheiro Excel escrever num ficheiro todos
 * os predicados que são necessários à base de conhecimento
 * @author Nelson Faria(A84727)
 * @version 1.0
 */

public class EscritaProlog {

    /* Váriável usada para guardar todos os objetos representativos das
    paragens de autocarros*/
    private List<Paragem> paragens;

    /* Váriável usada para guardar todos os objetos representativos das
    adjacencias, ou seja todas as carreiras presentes no ficheiro de adjacencias*/
    private List<Carreira> adjacencias;

    /* Variável usada para guardar o numero de ocorrencias de valores
    desconhecidos no ficheiro das Paragens*/
    private Map<String,List<String>> valoresDesconhecidosP;

    /* Variável usada para guardar o numero de ocorrencias de valores
    desconhecidos no ficheiro das Adjacencias*/
    private Map<String,List<String>> valoresDesconhecidosA;

    /* Nome do ficheiro Prolog onde vao estar os dados da base de conhecimento*/
    private static final String NOME_FICH_PAR = "paragensBC.prolog.pl";

    /* Nome do ficheiro Prolog onde vao estar os dados da base de conhecimento*/
    private static final String NOME_FICH_ADJ = "adjacenciasBC.prolog.pl";

    /**
     * Construtor por omissão da classe EscritaProlog que cria uma instância de
     * EscritaProlog sem serem necessários parâmetros
     * */
    public EscritaProlog(){
        this.paragens = new ArrayList<>();
        this.adjacencias = new ArrayList<>();
        this.valoresDesconhecidosP = new HashMap<>();
        this.valoresDesconhecidosA = new HashMap<>();
    }

    /**
     * Construtor parametrizado de EscritaProlog
     */
    public EscritaProlog(Map<String,List<String>> valoresDesconhecidosP, Map<String,List<String>> valoresDesconhecidosA, List<Paragem> paragens, List<Carreira> adjacencias){
        this.paragens = paragens;
        this.adjacencias = adjacencias;
        this.valoresDesconhecidosP = valoresDesconhecidosP;
        this.valoresDesconhecidosA = valoresDesconhecidosA;
    }

    /**
     * Método que dado uma latitude e uma longitude de cada uma das paragens
     * nos calcula a distancia eucliadiana entre as duas paragens
     * @param latitudeA
     * @param longitudeA
     * @param latitudeB
     * @param longitudeB
     * @return
     */
    public double distanciaEntreParagens(double latitudeA, double longitudeA,
                                         double latitudeB, double longitudeB){

        double distancia = 0;
        distancia = Math.sqrt(Math.pow((latitudeB-latitudeA),2) + Math.pow((longitudeB-longitudeA),2));
        return distancia;
    }

    /**
     * Método que serve para escrever os dados das paragens no ficheiro da base
     * de conhecimento
     * Formato: paragem( Gid,Lat.Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ).
     */
    public void escreveParagens(){

        try {
            // Abertura do ficheiro para escrita
            PrintWriter pw = new PrintWriter(NOME_FICH_PAR);
            /* Enquanto houverem paragens para escrever */
            for(Paragem p:this.paragens){
                // Preparacao da linha a inserir no ficheiro
                StringBuilder sb = new StringBuilder();
                sb.append("paragem( ");
                sb.append(p.getGid()); sb.append(",");
                sb.append(p.getLatitude()); sb.append(",");
                sb.append(p.getLongitude()); sb.append(",");
                sb.append(p.getEstado()); sb.append(",");
                sb.append(p.getTipo_Abrigo()); sb.append(",");
                if(p.isPublicidade()) sb.append("yes,");
                else sb.append("no,");
                sb.append(p.getOperadora()); sb.append(",");
                sb.append(p.getCarreira()); sb.append(",");
                sb.append(p.getCodigo_rua()); sb.append(",");
                if(!p.getNome_rua().contains("xptoSRCR")){
                    sb.append("'"); sb.append(p.getNome_rua()); sb.append("'");
                } else sb.append(p.getNome_rua());
                sb.append(",");
                if(!p.getFreguesia().contains("xptoSRCR")) {
                    sb.append("'"); sb.append(p.getFreguesia()); sb.append("'");
                } else sb.append(p.getFreguesia());
                sb.append(" ).");
                // Escrita da linha no ficheiro
                pw.println(sb.toString());
            }
            if(!this.valoresDesconhecidosP.isEmpty()){
                pw.println("\n% Excecoes relativas aos valores Desconhecidos presentes nesta base de conhecimento!");
                for(String tipo:this.valoresDesconhecidosP.keySet()){
                    List<String> lista = this.valoresDesconhecidosP.get(tipo);
                    for(String nomeDesc:lista){
                        switch (tipo){
                            case "Estado":
                                pw.println("excecao( paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ) ) :-\n\tparagem( Gid,Lat,Long,"+ nomeDesc +",Tipo,Pub,Oper,Carr,Cod,Nome,Freg ).");
                                break;
                            case "Tipo de Abrigo":
                                pw.println("excecao( paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ) ) :-\n\tparagem( Gid,Lat,Long,Estado,"+ nomeDesc +",Pub,Oper,Carr,Cod,Nome,Freg ).");
                                break;
                            case "Operadora":
                                pw.println("excecao( paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ) ) :-\n\tparagem( Gid,Lat,Long,Estado,Tipo,Pub,"+ nomeDesc +",Carr,Cod,Nome,Freg ).");
                                break;
                            case "Codigo da Rua":
                                pw.println("excecao( paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ) ) :-\n\tparagem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,"+ nomeDesc +",Nome,Freg ).");
                                break;
                            case "Nome da Rua":
                                pw.println("excecao( paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ) ) :-\n\tparagem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,"+ nomeDesc +",Freg ).");
                                break;
                            case "Freguesia":
                                pw.println("excecao( paragem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome,Freg ) ) :-\n\tparagem( Gid,Lat,Long,Estado,Tipo,Pub,Oper,Carr,Cod,Nome," + nomeDesc + " ).");
                                break;
                        }
                    }
                }
            }
            pw.close();
        } catch(IOException e){
            System.out.println("Erro de escrita no ficheiro de Paragens!!!");
        }
    }

    /**
     * Método que serve para escrever os dados das adjacencias no ficheiro da base
     * de conhecimento
     * Formato : adjacencia( Carreira,ParagemA,ParagemB,Distancia ).
     */
    public void escreveAdjacencias(){
        Paragem pAtual, pProx;
        try {
            // Abertura do ficheiro para escrita
            PrintWriter pw = new PrintWriter(NOME_FICH_ADJ);
            /* Enquanto houverem paragens para escrever */
            for(Carreira c:this.adjacencias){
                for(int i=0;i<c.getParagens().size()-1;i++) {
                    pAtual = c.getParagens().get(i);
                    pProx = c.getParagens().get(i+1);
                    // Preparacao da linha a inserir no ficheiro
                    StringBuilder sb = new StringBuilder();
                    sb.append("adjacencia( ");
                    sb.append(c.getId());
                    sb.append(",");
                    sb.append(pAtual.getGid());
                    sb.append(",");
                    sb.append(pProx.getGid());
                    sb.append(",");
                    double dist = this.distanciaEntreParagens(pAtual.getLatitude(),pAtual.getLongitude(),
                                                              pProx.getLatitude(),pProx.getLongitude());
                    sb.append(dist);
                    sb.append(" ).");
                    // Escrita da linha no ficheiro
                    pw.println(sb.toString());
                }
            }
            pw.close();
        } catch(IOException e){
            System.out.println("Erro de escrita no ficheiro de Adjacencias!!!");
        }
    }
}
