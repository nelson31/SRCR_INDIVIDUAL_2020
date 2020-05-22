import java.io.*;
import java.util.*;
import java.io.File;
import java.io.IOException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

/**         *********Sistemas de Representacao de Conhecimento e Raciocinio*********
 * Classe LeituraParagens usada para ler os dados do ficheiro excel
 * que contém as informações relativas ás paragens de Autocarros do
 * concelho de Oeiras. Aqui são criados os respétivos objetoos que
 * vão ser importantes para posteriormente reproduzir-se para o output
 * a respetiva base de conhecimento associada, com todas as linhas que serão
 * necessárias ao Prolog
 * @author Nelson Faria(A84727)
 * @version 1.0
 */

public class LeituraExcel {

    /* Váriável usada para guardar todos os objetos representativos das
    paragens de autocarros*/
    private List<Paragem> paragens;

    /* Váriável usada para guardar todos os objetos representativos das
    adjacencias, ou seja todas as carreiras presentes no ficheiro de adjacencias*/
    private List<Carreira> adjacencias;

    /* Variável usada para guardar todas as carreiras que existem */
    private Set<Integer> carreiras;

    /* Localização do ficheiro Excel das paragens de Oeiras*/
    private static final String PARAGENS_FILE = "./paragem_autocarros_oeiras_processado_4.xlsx";

    /* Localização do ficheiro Excel das paragens de Oeiras*/
    private static final String ADJACENCIAS_FILE = "./lista_adjacencias_paragens(3).xlsx";

    /*Numero de folhas no ficheiro de Adjacencia*/
    private static final int NUM_FOLHAS_ADJACENCIAS = 39;

    /* Variável usada para guardar o numero de ocorrencias de valores
    desconhecidos no ficheiro das Paragens*/
    private Map<String,List<String>> valoresDesconhecidosP;

    /* Variável usada para guardar o numero de ocorrencias de valores
    desconhecidos no ficheiro das Adjacencias*/
    private Map<String,List<String>> valoresDesconhecidosA;

    /* Variavel usada para contar o numero de valores Desconhecidos */
    private int numDesconhecidos;

    /**
     * Construtor por omissão da classe LeituraParagens que cria uma instância de
     * LeituraParagens sem serem necessários parâmetros
     * */
    public LeituraExcel(){
        this.paragens = new ArrayList<>();
        this.carreiras = new TreeSet<>();
        this.adjacencias = new ArrayList<>();
        this.valoresDesconhecidosP = new HashMap<>();
        this.valoresDesconhecidosA = new HashMap<>();
        this.numDesconhecidos = 0;
    }

    /**
     * Método que dado um determinado texto lhe retira os espaços
     * entre as palavras
     * @param texto
     * @return
     */
    public String retiraEspacos(String texto){

        StringBuilder sb = new StringBuilder();
        String[] aux = texto.split(" ");
        /*Enquanto existirem espaços em Branco para substituir*/
        for(int i=0;i<aux.length;i++){
            if(!aux[i].equals("") && i!=aux.length-1) {
                sb.append(aux[i]);
                sb.append("_");
            } else if(i==aux.length-1){
                sb.append(aux[i]);
            }
        }
        return sb.toString();
    }

    /**
     * Método que dada uma String presente no ficheiro Excel,
     * nos muda o encoding para UTF-8 e retira os espaços, substituindo-os
     * por hifens
     * @param input
     * @return
     */
    public String preparaString(String input){
        String output = "";
        try {
            byte array[] = input.getBytes("ISO-8859-1");
            output = new String(array, "UTF-8");
            output = this.retiraEspacos(output);
            output = output.toLowerCase();
            //System.out.println(output);
        } catch (IOException e) {
                System.out.println(e);
        }
        return output;
    }
    /**
     * Método usado para ler toda a informação contida no ficheiro Excel de Paragens
     * de Autocarros e criar os respetivos objetos paragens
     */
    public void leituraFileParagens(){

        int numParagens = 0;
        String aux = "";
        List<String> lAux;
        try {
            // obter o apontador para o ficheiro
            FileInputStream arquivo = new FileInputStream(new File(
                    PARAGENS_FILE));

            // Objeto com o ficheiro excel
            XSSFWorkbook workbook = new XSSFWorkbook(arquivo);

            // Folha do Excel com indice 0
            XSSFSheet sheetParagens = workbook.getSheetAt(0);

            /* Enquanto houver linhas para ler*/
            for (Row row : sheetParagens) {
                if(numParagens>0) {
                    // Serve para iterar sobre as colunas da folha
                    Paragem paragem = new Paragem();
                    /* Enquanto houver colunas para ler*/
                    for (Cell cell : row) {
                        switch (cell.getColumnIndex()) {
                            case 0:
                                // Gid
                                paragem.setGid(Integer.parseInt(cell.getStringCellValue()));
                                break;
                            case 1:
                                // Latitude
                                paragem.setLatitude(Double.parseDouble(cell.getStringCellValue()));
                                break;
                            case 2:
                                // Longitude
                                paragem.setLongitude(Double.parseDouble(cell.getStringCellValue()));
                                break;
                            case 3:
                                // Estado de Conservacao
                                if((aux = cell.getStringCellValue()).equals("")) {
                                    // Tratamento dos Valores Desconhecidos
                                    paragem.setEstado("xptoSRCR" + this.numDesconhecidos);
                                    if((lAux = this.valoresDesconhecidosP.get("Estado"))==null)
                                        lAux = new ArrayList<>();
                                    lAux.add("xptoSRCR" + this.numDesconhecidos);
                                    this.valoresDesconhecidosP.remove("Estado");
                                    this.valoresDesconhecidosP.put("Estado",lAux);
                                    this.numDesconhecidos++;
                                } else {
                                    paragem.setEstado(this.preparaString(aux));
                                }
                                break;
                            case 4:
                                // Tipo de Abrigo
                                if((aux = cell.getStringCellValue()).equals("")) {
                                    // Tratamento dos Valores Desconhecidos
                                    paragem.setTipo_Abrigo("xptoSRCR" + this.numDesconhecidos);
                                    if((lAux = this.valoresDesconhecidosP.get("Tipo de Abrigo"))==null)
                                        lAux = new ArrayList<>();
                                    lAux.add("xptoSRCR" + this.numDesconhecidos);
                                    this.valoresDesconhecidosP.remove("Tipo de Abrigo");
                                    this.valoresDesconhecidosP.put("Tipo de Abrigo",lAux);
                                    this.numDesconhecidos++;
                                } else {
                                    paragem.setTipo_Abrigo(this.preparaString(aux));
                                }
                                break;
                            case 5:
                                // Abrigo com Publicidade
                                boolean pub = false;
                                if(cell.getStringCellValue().equals("Yes")) pub = true;
                                paragem.setPublicidade(pub);
                                break;
                            case 6:
                                // Operadora
                                if((aux = cell.getStringCellValue()).equals("")) {
                                    // Tratamento dos Valores Desconhecidos
                                    paragem.setOperadora("xptoSRCR" + this.numDesconhecidos);
                                    if((lAux = this.valoresDesconhecidosP.get("Operadora"))==null)
                                        lAux = new ArrayList<>();
                                    lAux.add("xptoSRCR" + this.numDesconhecidos);
                                    this.valoresDesconhecidosP.remove("Operadora");
                                    this.valoresDesconhecidosP.put("Operadora",lAux);
                                    this.numDesconhecidos++;
                                } else {
                                    paragem.setOperadora(this.preparaString(aux));
                                }
                                break;
                            case 7:
                                // Carreira
                                String value = cell.getStringCellValue();
                                String[] values = value.split(",");
                                List<Integer> carrs = new ArrayList<>();
                                for (String s : values) {
                                    carrs.add(Integer.parseInt(s));
                                    this.carreiras.add(Integer.parseInt(s));
                                }
                                paragem.setCarreira(carrs);
                                break;
                            case 8:
                                // Codigo da Rua
                                if((aux = String.valueOf(cell.getNumericCellValue())).equals("")) {
                                    // Tratamento dos Valores Desconhecidos
                                    paragem.setCodigo_rua("xptoSRCR" + this.numDesconhecidos);
                                    if((lAux = this.valoresDesconhecidosP.get("Codigo da Rua"))==null)
                                        lAux = new ArrayList<>();
                                    lAux.add("xptoSRCR" + this.numDesconhecidos);
                                    this.valoresDesconhecidosP.remove("Codigo da Rua");
                                    this.valoresDesconhecidosP.put("Codigo da Rua",lAux);
                                    this.numDesconhecidos++;
                                } else {
                                    paragem.setCodigo_rua(this.preparaString(aux));
                                }
                                break;
                            case 9:
                                // Nome da Rua
                                if((aux = cell.getStringCellValue()).equals("")) {
                                    // Tratamento dos Valores Desconhecidos
                                    paragem.setNome_rua("xptoSRCR" + this.numDesconhecidos);
                                    if((lAux = this.valoresDesconhecidosP.get("Nome da Rua"))==null)
                                        lAux = new ArrayList<>();
                                    lAux.add("xptoSRCR" + this.numDesconhecidos);
                                    this.valoresDesconhecidosP.remove("Nome da Rua");
                                    this.valoresDesconhecidosP.put("Nome da Rua",lAux);
                                    this.numDesconhecidos++;
                                } else {
                                    paragem.setNome_rua(this.preparaString(aux));
                                }
                                break;
                            case 10:
                                // Freguesia
                                if((aux = cell.getStringCellValue()).equals("")) {
                                    // Tratamento dos Valores Desconhecidos
                                    paragem.setFreguesia("xptoSRCR" + this.numDesconhecidos);
                                    if((lAux = this.valoresDesconhecidosP.get("Freguesia"))==null)
                                        lAux = new ArrayList<>();
                                    lAux.add("xptoSRCR" + this.numDesconhecidos);
                                    this.valoresDesconhecidosP.remove("Freguesia");
                                    this.valoresDesconhecidosP.put("Freguesia",lAux);
                                    this.numDesconhecidos++;
                                } else {
                                    paragem.setFreguesia(this.preparaString(aux));
                                }
                                break;
                        }
                    }
                    this.paragens.add(paragem);
                }
                numParagens++;
            }
            arquivo.close();

            /*for(Paragem p : paragens)
                System.out.println(p.toString());*/

            // Nem todas as carreiras tem correspondencia no adjacencias!!
            /*for(Integer s : this.carreiras)
                System.out.println("Carreira : " + s);*/

            System.out.println("Número Total de Paragens: " + numParagens);
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println("Arquivo Excel não encontrado!");
        }
    }

    /**
     * Método usado para ler toda a informação contida no ficheiro Excel de listas
     * de Adjacencias de Paragens e criar os respetivos objetos carreiras
     */
    public void leituraFileAdjacencias(){
        int numParagens = 0;
        String aux = "";
        List<String> lAux;
        try {
            // obter o apontador para o ficheiro
            FileInputStream arquivo = new FileInputStream(new File(
                    ADJACENCIAS_FILE));

            // Objeto com o ficheiro excel
            XSSFWorkbook workbook = new XSSFWorkbook(arquivo);

            XSSFSheet sheetParagens;
            /*Percorro todas as folhas do ficheiro*/
            for(int i = 0;i<NUM_FOLHAS_ADJACENCIAS; i++) {
                sheetParagens = workbook.getSheetAt(i);
                if (this.carreiras.contains(Integer.parseInt(sheetParagens.getSheetName()))) {
                    Carreira carreira = new Carreira();
                    carreira.setId(Integer.parseInt(sheetParagens.getSheetName()));
                    /* Enquanto houver linhas para ler*/
                    for (Row row : sheetParagens) {
                        Paragem paragem = new Paragem();
                        if (numParagens > 0) {
                            /* Enquanto houver colunas para ler*/
                            for (Cell cell : row) {
                                switch (cell.getColumnIndex()) {
                                    case 0:
                                        // Gid
                                        paragem.setGid(Integer.parseInt(cell.getStringCellValue()));
                                        break;
                                    case 1:
                                        // Latitude
                                        paragem.setLatitude(Double.parseDouble(cell.getStringCellValue()));
                                        break;
                                    case 2:
                                        // Longitude
                                        paragem.setLongitude(Double.parseDouble(cell.getStringCellValue()));
                                        break;
                                    case 3:
                                        // Estado de Conservacao
                                        if((aux = cell.getStringCellValue()).equals("")) {
                                            // Tratamento dos Valores Desconhecidos
                                            paragem.setEstado("xptoSRCR" + this.numDesconhecidos);
                                            if((lAux = this.valoresDesconhecidosA.get("Estado"))==null)
                                                lAux = new ArrayList<>();
                                            lAux.add("xptoSRCR" + this.numDesconhecidos);
                                            this.valoresDesconhecidosP.remove("Estado");
                                            this.valoresDesconhecidosA.put("Estado",lAux);
                                            this.numDesconhecidos++;
                                        } else {
                                            paragem.setEstado(this.preparaString(aux));
                                        }
                                        break;
                                    case 4:
                                        // Tipo de Abrigo
                                        if((aux = cell.getStringCellValue()).equals("")) {
                                            // Tratamento dos Valores Desconhecidos
                                            paragem.setTipo_Abrigo("xptoSRCR" + this.numDesconhecidos);
                                            if((lAux = this.valoresDesconhecidosA.get("Tipo de Abrigo"))==null)
                                                lAux = new ArrayList<>();
                                            lAux.add("xptoSRCR" + this.numDesconhecidos);
                                            this.valoresDesconhecidosA.remove("Tipo de Abrigo");
                                            this.valoresDesconhecidosA.put("Tipo de Abrigo",lAux);
                                            this.numDesconhecidos++;
                                        } else {
                                            paragem.setTipo_Abrigo(this.preparaString(aux));
                                        }
                                        break;
                                    case 5:
                                        // Abrigo com Publicidade
                                        boolean pub = false;
                                        if (cell.getStringCellValue().equals("Yes")) pub = true;
                                        paragem.setPublicidade(pub);
                                        break;
                                    case 6:
                                        // Operadora
                                        if((aux = cell.getStringCellValue()).equals("")) {
                                            // Tratamento dos Valores Desconhecidos
                                            paragem.setOperadora("xptoSRCR" + this.numDesconhecidos);
                                            if((lAux = this.valoresDesconhecidosA.get("Operadora"))==null)
                                                lAux = new ArrayList<>();
                                            lAux.add("xptoSRCR" + this.numDesconhecidos);
                                            this.valoresDesconhecidosA.remove("Operadora");
                                            this.valoresDesconhecidosA.put("Operadora",lAux);
                                            this.numDesconhecidos++;
                                        } else {
                                            paragem.setOperadora(this.preparaString(aux));
                                        }
                                        break;
                                    case 7:
                                        // Carreira
                                        String value = cell.getStringCellValue();
                                        String[] values = value.split(",");
                                        List<Integer> carreiras = new ArrayList<>();
                                        for (String s : values)
                                            carreiras.add(Integer.parseInt(s));
                                        paragem.setCarreira(carreiras);
                                        break;
                                    case 8:
                                        // Codigo da Rua
                                        if((aux = String.valueOf(cell.getNumericCellValue())).equals("")) {
                                            // Tratamento dos Valores Desconhecidos
                                            paragem.setCodigo_rua("xptoSRCR" + this.numDesconhecidos);
                                            if((lAux = this.valoresDesconhecidosA.get("Codigo da Rua"))==null)
                                                lAux = new ArrayList<>();
                                            lAux.add("xptoSRCR" + this.numDesconhecidos);
                                            this.valoresDesconhecidosA.remove("Codigo da Rua");
                                            this.valoresDesconhecidosA.put("Codigo da Rua",lAux);
                                            this.numDesconhecidos++;
                                        } else {
                                            paragem.setCodigo_rua(this.preparaString(aux));
                                        }
                                        break;
                                    case 9:
                                        // Nome de Rua
                                        if((aux = cell.getStringCellValue()).equals("")) {
                                            // Tratamento dos Valores Desconhecidos
                                            paragem.setNome_rua("xptoSRCR" + this.numDesconhecidos);
                                            if((lAux = this.valoresDesconhecidosA.get("Nome da Rua"))==null)
                                                lAux = new ArrayList<>();
                                            lAux.add("xptoSRCR" + this.numDesconhecidos);
                                            this.valoresDesconhecidosA.remove("Nome da Rua");
                                            this.valoresDesconhecidosA.put("Nome da Rua",lAux);
                                            this.numDesconhecidos++;
                                        } else {
                                            paragem.setNome_rua(this.preparaString(aux));
                                        }
                                        break;
                                    case 10:
                                        // Freguesia
                                        if((aux = cell.getStringCellValue()).equals("")) {
                                            // Tratamento dos Valores Desconhecidos
                                            paragem.setFreguesia("xptoSRCR" + this.numDesconhecidos);
                                            if((lAux = this.valoresDesconhecidosA.get("Freguesia"))==null)
                                                lAux = new ArrayList<>();
                                            lAux.add("xptoSRCR" + this.numDesconhecidos);
                                            this.valoresDesconhecidosA.remove("Freguesia");
                                            this.valoresDesconhecidosA.put("Freguesia",lAux);
                                            this.numDesconhecidos++;
                                        } else {
                                            paragem.setFreguesia(this.preparaString(aux));
                                        }
                                        break;
                                }
                            }
                            carreira.addParagem(paragem);
                        }
                        numParagens++;
                        //System.out.println("Paragens: " + numParagens);
                    }
                    this.adjacencias.add(carreira);
                }
                numParagens=0;
            }
            arquivo.close();

        } catch (IOException e) {
            e.printStackTrace();
            System.out.println("Arquivo Excel não encontrado!");
        }
    }

    // Getters e Setters

    /**
     * Método que devolve a lista de paragens
     * @return paragens
     */
    public List<Paragem> getParagens() {
        return paragens;
    }

    /**
     * Método que atribui um novo valor à variável paragens
     * @param paragens
     */
    public void setParagens(List<Paragem> paragens) {
        this.paragens = paragens;
    }

    /**
     * Método que devolve a lista de adjacencias
     * @return adjacencias
     */
    public List<Carreira> getAdjacencias() {
        return adjacencias;
    }

    /**
     * Método que atribui um novo valor à variável adjacencias
     * @param adjacencias
     */
    public void setAdjacencias(List<Carreira> adjacencias) {
        this.adjacencias = adjacencias;
    }

    /**
     * Método que devolve a correspondencia entre o campo do predicado e os
     * respetivos nomes dos valores desconhecidos do ficheiro das Paragens
     * @return valoresDesconhecidosP
     */
    public Map<String, List<String>> getValoresDesconhecidosP() {
        return valoresDesconhecidosP;
    }

    /**
     * Método que atribui um novo valor à variável valoresDesconhecidosP
     * @param valoresDesconhecidosP
     */
    public void setValoresDesconhecidosP(Map<String, List<String>> valoresDesconhecidosP) {
        this.valoresDesconhecidosP = valoresDesconhecidosP;
    }

    /**
     * Método que devolve a correspondencia entre o campo do predicado e os
     * respetivos nomes dos valores desconhecidos do ficheiro das Adjacencias
     * @return valoresDesconhecidosA
     */
    public Map<String, List<String>> getValoresDesconhecidosA() {
        return valoresDesconhecidosA;
    }

    /**
     * Método que atribui um novo valor à variável valoresDesconhecidosA
     * @param valoresDesconhecidosA
     */
    public void setValoresDesconhecidosA(Map<String, List<String>> valoresDesconhecidosA) {
        this.valoresDesconhecidosA = valoresDesconhecidosA;
    }
}


