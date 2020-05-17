import java.util.*;

/**         *********Sistemas de Representacao de Conhecimento e Raciocinio*********
 *  Classe Paragem usada para guardar todas as informações relativas
 *  a uma paragem de autocarros. Está é a informação que está incluída
 *  no dataset Paragens de Autocarro
 *  @author Nelson Faria(A84727)
 *  @version 1.0
 */

public class Paragem {

    /* Váriável usada para guardar a informação da latitude onde a paragem
    se encontra */
    private double latitude;

    /* Váriável usada para guardar a informação da longitude onde a paragem
    se encontra */
    private double longitude;

    /* Variável usada para guardar o identificador de um ponto no mapa(gid)*/
    private int gid;

    /* Variável usada para guardar informação sobre o estado de conservação
    da paragem*/
    private String estado;

    /* Variável usada para guardar informação sobre o tipo de Abrigo
    da paragem*/
    private String tipo_Abrigo;

    /* Variável usada para guardar informação sobre se a paragem é um abrigo
    com publicidade ou não*/
    private boolean publicidade;

    /* Variável usada para guardar informação sobre a operadora da paragem*/
    private String operadora;

    /* Variável usada para guardar informação sobre as carreiras a que a paragem
    pertence*/
    private List<Integer> carreira;

    /* Variável usada para guardar informação sobre o código da rua onde a paragem
    se situa */
    private String codigo_rua;

    /* Variável usada para guardar o nome da rua onde a pparagem se encontra*/
    private String nome_rua;

    /* Variável usada para guardar o nome da freguesia onde a pparagem se encontra*/
    private String freguesia;

    /**
     * Construtor por omissão da classe Paragem que cria uma instância de
     * Paragem sem serem necessários parâmetros
     */
    public Paragem(){
        this.latitude = 0.0f;
        this.longitude = 0.0f;
        this.gid = 0;
        this.estado = "";
        this.tipo_Abrigo = "";
        this.publicidade=false;
        this.operadora = "";
        this.carreira = new ArrayList<>();
        this.codigo_rua = "";
        this.nome_rua = "";
        this.freguesia = "";
    }

    /**
     * Construtor parametrizado da classe Paragem que cria uma instância de
     * Paragem a partir dos parâmetros recebidos como argumento
     * @param latitude
     * @param longitude
     * @param gid
     * @param estado
     * @param tipoAbrigo
     * @param publicidade
     * @param operadora
     * @param carreira
     * @param codigo_rua
     * @param nome_rua
     * @param freguesia
     */
    public Paragem(double latitude, double longitude, int gid,
                   String estado, String tipoAbrigo, boolean publicidade, String operadora,
                   List<Integer> carreira, String codigo_rua, String nome_rua, String freguesia) {
        this.latitude = latitude;
        this.longitude = longitude;
        this.gid = gid;
        this.estado = estado;
        this.tipo_Abrigo = tipoAbrigo;
        this.publicidade = publicidade;
        this.operadora = operadora;
        this.carreira = carreira;
        this.codigo_rua = codigo_rua;
        this.nome_rua = nome_rua;
        this.freguesia = freguesia;
    }

    /* GETTERS E SETTERS*/

    /**
     * Método que devolve a latitude da paragem
     * @return latitude
     */
    public double getLatitude() {
        return this.latitude;
    }

    /**
     * Método que devolve a longitude da paragem
     * @return longitude
     */
    public double getLongitude() {
        return this.longitude;
    }

    /**
     * Método que devolve a gid da paragem
     * @return gid
     */
    public int getGid() {
        return this.gid;
    }

    /**
     * Método que devolve o estado da paragem
     * @return estado
     */
    public String getEstado() {
        return this.estado;
    }

    /**
     * Método que devolve o tipo de Abrigo da Paragem
     * @return tipo
     */
    public String getTipo_Abrigo() {
        return this.tipo_Abrigo;
    }

    /**
     * Método que nos diz se a paragem é um abrigo com publicidade ou não
     * @return true ou false
     */
    public boolean isPublicidade() {
        return this.publicidade;
    }

    /**
     * Método que devolve a operadora da paragem
     * @return operadora
     */
    public String getOperadora() {
        return this.operadora;
    }

    /**
     * Método que devolve as carreiras a que a paragem pertence
     * @return carreira
     */
    public List<Integer> getCarreira() {
        return this.carreira;
    }

    /**
     * Método que devolve o codigo da rua da paragem
     * @return codigo_rua
     */
    public String getCodigo_rua() {
        return this.codigo_rua;
    }

    /**
     * Método que devolve o nome da rua da paragem
     * @return nome_rua
     */
    public String getNome_rua() {
        return this.nome_rua;
    }

    /**
     * Método que devolve a freguesia da paragem
     * @return freguesia
     */
    public String getFreguesia() {
        return this.freguesia;
    }

    /**
     * Método que atribui um novo valor à variável latitude
     * @param latitude
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Método que atribui um novo valor à variável longitude
     * @param longitude
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /**
     * Método que atribui um novo valor à variável gid
     * @param gid
     */
    public void setGid(int gid) {
        this.gid = gid;
    }

    /**
     * Método que atribui um novo valor à variável estado
     * @param estado
     */
    public void setEstado(String estado) {
        this.estado = estado;
    }

    /**
     * Método que atribui um novo valor à variável tipo de Abrigo
     * @param tipoAbrigo
     */
    public void setTipo_Abrigo(String tipoAbrigo) {
        this.tipo_Abrigo = tipoAbrigo;
    }

    /**
     * Método que atribui um novo valor à variável publicidade
     * @param publicidade
     */
    public void setPublicidade(boolean publicidade) {
        this.publicidade = publicidade;
    }

    /**
     * Método que atribui um novo valor à variável operadora
     * @param operadora
     */
    public void setOperadora(String operadora) {
        this.operadora = operadora;
    }

    /**
     * Método que atribui um novo valor à variável carreira
     * @param carreira
     */
    public void setCarreira(List<Integer> carreira) {
        this.carreira = carreira;
    }

    /**
     * Método que atribui um novo valor à variável codigo_rua
     * @param codigo_rua
     */
    public void setCodigo_rua(String codigo_rua) {
        this.codigo_rua = codigo_rua;
    }

    /**
     * Método que atribui um novo valor à variável nome_rua
     * @param nome_rua
     */
    public void setNome_rua(String nome_rua) {
        this.nome_rua = nome_rua;
    }

    /**
     * Método que atribui um novo valor à variável freguesia
     * @param freguesia
     */
    public void setFreguesia(String freguesia) {
        this.freguesia = freguesia;
    }

    /**
     * Método usado para comparar a Paragem com o objeto recebido como parametro
     * @param o
     * @return true se são iguais
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Paragem paragem = (Paragem) o;
        return Double.compare(paragem.latitude, latitude) == 0 &&
                Double.compare(paragem.longitude, longitude) == 0 &&
                gid == paragem.gid &&
                publicidade == paragem.publicidade &&
                estado.equals(paragem.estado) &&
                operadora.equals(paragem.operadora) &&
                carreira.equals(paragem.carreira) &&
                codigo_rua.equals(paragem.codigo_rua) &&
                nome_rua.equals(paragem.nome_rua) &&
                freguesia.equals(paragem.freguesia);
    }

    /**
     * Método que nos devolve um hash do objeto paragem
     * @return
     */
    public int hashCode() {
        return Objects.hash(latitude, longitude, gid, estado, publicidade, operadora, carreira, codigo_rua, nome_rua, freguesia);
    }

    /**
     * Método usado para imprimir a informação relativa à paragem
     * @return
     */
    public String toString() {
        int num=0;
        StringBuilder sb = new StringBuilder();
        sb.append("\n[Paragem] : ");
        sb.append("\nlatitude = "); sb.append(this.latitude);
        sb.append("\nlongitude = "); sb.append(this.longitude);
        sb.append("\ngid = "); sb.append(this.gid);
        sb.append("\nestado = "); sb.append(this.estado);
        sb.append("\npublicidade = "); sb.append(this.publicidade);
        sb.append("\noperadora = "); sb.append(this.operadora);
        sb.append("\nCarreiras = ");
        for(Integer i:this.carreira) {
            num++;
            sb.append("\n Número da Carreira nº" + num + " = ");
            sb.append(this.operadora);
        }
        sb.append("\ncodigo_rua = "); sb.append(this.codigo_rua);
        sb.append("\nnome_rua = "); sb.append(this.nome_rua);
        sb.append("\nfreguesia = "); sb.append(this.freguesia);
        return sb.toString();
    }

}
