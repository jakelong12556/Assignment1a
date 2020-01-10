package com.bham.pij.assignments.a2a;

import java.util.Scanner;
import java.util.ArrayList;
import java.lang.Object;
import java.util.Random;

public class Individual implements Comparable {
    private String allowedValues;
    private String extendedValues;
    //private String chromosome;
    private ArrayList<Character> genes;
    private int upperBound;
    private int chromosomeNum;
    private double fitness;
    private Random value;

    public Individual(int upperBound, int chromosomeNum) {
        this.upperBound = upperBound;
        this.chromosomeNum = chromosomeNum;
        allowedValues = "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ ,.-?";
        extendedValues = allowedValues + allowedValues;
        value = new Random();
        genes = new ArrayList<Character>(chromosomeNum);

    }

    public Individual(Individual individual) {
        genes = individual.genes;
        fitness = individual.getFitness();
        upperBound = individual.upperBound;
    }

    public Individual(int chromosomeNum) {
        this.chromosomeNum = chromosomeNum;
        allowedValues = "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ ,.-?";
        extendedValues = allowedValues + allowedValues;
        value = new Random();
        genes = new ArrayList<Character>(chromosomeNum);

    }

    public double getFitness() {
        return fitness;
    }

    @Override
    public int compareTo(Object compareIndv) {
        Individual indv = (Individual) compareIndv;
        int compareFit = (int)(indv.getFitness());
        return (int)(this.fitness)-compareFit;
    }


    public String toString() {
        String chromosome = "";
        for (int i = 0; i < genes.size(); i++) {
            char tobeStringed = genes.get(i);
            chromosome += tobeStringed;
        }
        return chromosome;
    }

    public void setGenes(String chromosome) {//shared method, used by all GAApplication
        genes.clear();
        for(int j = 0; j < chromosomeNum; j++){
            char foundValue = chromosome.charAt(j);
            genes.add(foundValue);
        }
    }

    public void newChromosome() {//crete new individual with set parameters
        for(int j = 0; j < chromosomeNum; j++){
            char randomValue = allowedValues.charAt(value.nextInt(upperBound));
            genes.add(randomValue);
        }
    }

    public void setFitnessBinary() {//find the fitness of this Individual BinaryMaximiser
        fitness = 0;

        for(int i = 0; i < chromosomeNum; i++) {
            char geneFitness = genes.get(i);
            if (geneFitness == '1') {
                fitness += 1;
            }
        }
    }

    public void setFitnessWeasel(String weaselvalue) {//find fitness of Indv Weasel
        fitness = 0;//initialise

        for (int i = 0; i < chromosomeNum; i++) {
            double fitnessChar = 0;
            double goalCharacter = allowedValues.indexOf(weaselvalue.charAt(i));
            double givenChar = allowedValues.indexOf(genes.get(i));
            double givenCharLastIndex = extendedValues.lastIndexOf(genes.get(i));

            double difference = Math.abs(goalCharacter - givenChar);
            Double backwardsDifference = Math.abs(goalCharacter - givenCharLastIndex);

            if (difference > backwardsDifference) {
                fitnessChar = 67.0 - backwardsDifference;
            } else {
                fitnessChar = 67.0 - difference;
            }
            fitness += fitnessChar;
        }
    }

    public void mutateGenesBinary(double mutateChance) {
        for (int i = 0; i < genes.size(); i++) {
            double isMutate = value.nextDouble();

            if (isMutate <= mutateChance){
            char binaryValue = genes.get(i);
                if (binaryValue == '0') {
                    binaryValue = '1';
                    genes.set(i, binaryValue);
                } else {
                    binaryValue = '0';
                    genes.set(i, binaryValue);
                }
            }
        }

    }

    public void mutateGeneWeasel(double mutateChance, String weaselvalue) {

        for (int i = 0; i < genes.size(); i++) {
            double isMutate = value.nextDouble();
            if (isMutate <= mutateChance){
                char weaselChar = weaselvalue.charAt(i);

                int goalCharacter = extendedValues.indexOf(weaselChar);
                int givenChar = extendedValues.indexOf(genes.get(i));
                int givenCharLastIndex = extendedValues.lastIndexOf(genes.get(i));

                int difference = Math.abs(goalCharacter - givenChar);
                int backwardsDifference = Math.abs(goalCharacter - givenCharLastIndex);

                if (difference > backwardsDifference) {
                    givenCharLastIndex--;
                    weaselChar = extendedValues.charAt(givenCharLastIndex);
                    genes.set(i, weaselChar);
                } else {
                    difference = goalCharacter - givenChar;
                    if (difference < 0) {
                        givenChar--;
                        if (givenChar == -1) {
                            givenChar = 66;
                        }
                        weaselChar = allowedValues.charAt(givenChar);
                        genes.set(i, weaselChar);
                    } else {
                        givenChar++;
                        if (givenChar == 67) {
                            givenChar = 0;
                        }
                        weaselChar = allowedValues.charAt(givenChar);
                        genes.set(i, weaselChar);
                    }
                }

            }

        }

    }

}
