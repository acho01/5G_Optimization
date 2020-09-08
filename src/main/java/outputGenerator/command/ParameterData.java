package outputGenerator.command;

import java.util.List;

public class ParameterData {
    private List<String> radiiList;

    private List<String> epsilonList;

    private List<String> distanceList;

    private List<String> betaDataList;

    private List<String> alphaDataList;

    private List<String> frequencyDataList;

    private String fileName;

    private String delta;

    public ParameterData() {
    }

    public List<String> getRadiiList() {
        return radiiList;
    }

    public void setRadiiList(List<String> radiiList) {
        this.radiiList = radiiList;
    }

    public List<String> getEpsilonList() {
        return epsilonList;
    }

    public void setEpsilonList(List<String> epsilonList) {
        this.epsilonList = epsilonList;
    }

    public List<String> getDistanceList() {
        return distanceList;
    }

    public void setDistanceList(List<String> distanceList) {
        this.distanceList = distanceList;
    }

    public List<String> getBetaDataList() {
        return betaDataList;
    }

    public void setBetaDataList(List<String> betaDataList) {
        this.betaDataList = betaDataList;
    }

    public List<String> getAlphaDataList() {
        return alphaDataList;
    }

    public void setAlphaDataList(List<String> alphaDataList) {
        this.alphaDataList = alphaDataList;
    }

    public List<String> getFrequencyDataList() {
        return frequencyDataList;
    }

    public void setFrequencyDataList(List<String> frequencyDataList) {
        this.frequencyDataList = frequencyDataList;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public String getDelta() {
        return delta;
    }

    public void setDelta(String delta) {
        this.delta = delta;
    }
}
