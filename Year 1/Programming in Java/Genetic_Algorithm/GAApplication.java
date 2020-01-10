package com.bham.pij.assignments.a2a.GAApplication;

import java.util.Scanner;
import java.util.ArrayList;
import java.lang.Object;
import java.util.Random;
import java.util.*;

public class GAApplication {
    protected int No_population;//size of the population made up of individuals
    protected int No_Chromosomes;//number of char gene in each Individual
    protected String weaselString;
    protected int No_Parent;
    protected double crossChance;
    protected double mutateChance;
    protected ArrayList<Individual> population;
    protected ArrayList<Individual> parentPopulation;
    protected ArrayList<Individual> childpopulation;

    protected Random value;

    public GAApplication() {
        population = new ArrayList<Individual>();
        parentPopulation = new ArrayList<Individual>();
        childpopulation = new ArrayList<Individual>();
        value = new Random();
    }

    //helping method TODO remove before submit
    public void printPopulationAndFitness() {
       String tempstring = "";
       String tempstring1 = "";

       for (int i = 0; i < population.size(); i++) {

           Individual temp = population.get(i);
           tempstring = temp.toString();
           temp.setFitnessBinary();
           System.out.println(tempstring + " " + temp.getFitness());
           tempstring = "";
       }

    }

    public void run() {

    }

    public Individual getBest() {
        Individual bestIndividual = population.get(population.size() - 1);

        return bestIndividual;
    }

    public void newPopulation() { //create population of individual from a-z
    }

    public void mutationOperation() { //mutate population
    }

    public void sortPopulationFitLv() { //sort population
    }

    public void setPopulationFit() {
        for (int i = 0; i < No_population; i++) {
            Individual setPopulationFit = population.get(i);
            setPopulationFit.setFitnessBinary();
        }
    }

    public void crossover() {//perform crossover and make the new children
    }

}
