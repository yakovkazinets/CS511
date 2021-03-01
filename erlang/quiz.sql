CREATE TABLE Drug (
    trade_name varchar(255),   
    formula varchar(255),   
    FOREIGN KEY (Makes) REFERENCES Pharm_co(name));

CREATE TABLE Sell (  
    price Integer,  
    FOREIGN KEY (PharmacyName) 
    REFERENCES Pharmacy(name),  
    FOREIGN KEY (Drug) REFERENCES Drug(trade_name));

CREATE TABLE Prescription (
    quantity Integer,  
    date Date,  
    FOREIGN KEY (PatientSsn) REFERENCES Patient(ssn),  
    FOREIGN KEY (DoctorSsn) REFERENCES Doctor(phy_ssn))

SELECT M.annualIncome
FROM Musicians M
WHERE M.annualIncome = (SELECT MAX(annualIncome) FROM Musicians)

SELECT T.title
FROM AlbumProducer T, Instruments I, Plays P
WHERE T.ssn = P.ssn and P.instrID = I.instrID and I.iname = 'guitar'
INTERSECT
SELECT T.title
FROM AlbumProducer T, Instruments I, Plays P
WHERE T.ssn = P.ssn and P.instrID = I.instrID and I.iname = 'piano'

SELECT M.name
FROM Musicians M, AlbumProducer A, SongsAppear S, 
WHERE M.ssn = A.ssn and A.albumID = S.albumID
GROUP BY M.ssn, A.albumID, S.songID
HAVING COUNT(A.albumID) >= 6