import java.util.ArrayList;
import java.util.List;

/**         *********Sistemas de Representacao de Conhecimento e Raciocinio*********
 *  Classe Carreira que serve para guardar informação sobre uma carreira em específico,
 *  ou seja, a sucessão de paragens por onde uma carreira passa
 *  @author Nelson Faria(A84727)
 *  @version 1.0
 */

public class Carreira {

    /* Variável que serve para guardar o identificador da carreira*/
    private int id;

    /* Variável que serve para guardar as paragens que fazem parte do trajeto*/
    private List<Paragem> paragens;

    /**
     * Construtor por omissão da classe Carreira que cria uma instância de
     * Carreira sem serem necessários parâmetros
     */
    public Carreira(){
        this.id = -1;
        this.paragens = new ArrayList<>();
    }

    /**
     * Construtor parametrizado da classe Carreira que cria uma instância de
     * Carreira a partir dos parâmetros recebidos como argumento
     * @param id
     * @param paragens
     */
    public Carreira(int id, List<Paragem> paragens) {
        this.id = id;
        this.paragens = paragens;
    }

    // Getters e Setters

    /**
     * Método que devolve o id da carreira
     * @return id
     */
    public int getId() {
        return id;
    }

    /**
     * Método que devolve a lista de paragens
     * @return paragens
     */
    public List<Paragem> getParagens() {
        return paragens;
    }

    /**
     * Método que atribui um novo valor à variável id
     * @param id
     */
    public void setId(int id) {
        this.id = id;
    }

    /**
     * Método que atribui um novo valor à variável paragens
     * @param paragens
     */
    public void setParagens(List<Paragem> paragens) {
        this.paragens = paragens;
    }

    /**
     * Método que serve para adicionar uma nova paragem à carreira
     * @param par
     */
    public void addParagem(Paragem par){
        this.paragens.add(par);
    }

}
